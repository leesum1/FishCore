package leesum
import chisel3._
import chisel3.util.{Decoupled, Enum, Queue, is, switch}
class LoadQueueIn extends Bundle {
  val paddr = UInt(64.W)
  // 0: 1 byte, 1: 2 bytes, 2: 4 bytes, 3: 8 bytes
  val size = UInt(2.W)
  val is_mmio = Bool()
  val sign_ext = Bool()
  val trans_id = UInt(32.W)
  val store_bypass = new StoreBypassData
}

class LoadWriteBack extends Bundle {
  val rdata = UInt(64.W)
  val tran_id = UInt(32.W)
}

class LoadQueue extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LoadQueueIn))
    val load_wb = Decoupled(new LoadWriteBack)
    // flush and mmio_commit should not be true at the same time
    val flush = Input(Bool())
    // from commit stage, when commit a mmio instruction, set mmio_commit to true
    val mmio_commit = Flipped(Decoupled(Bool()))
    val dcache_req = Decoupled(new LoadDcacheReq)
    val dcache_resp = Flipped(Decoupled(new LoadDcacheResp))
  })
  val load_queue_size = 4
  val load_queue_out = Queue(
    io.in,
    load_queue_size,
    pipe = true,
    flow = true,
    flush = Some(io.flush)
  )

  // push a pipeline to the output
  val wb_pipe = Module(new PipeLine(new LoadWriteBack))
  wb_pipe.io.out <> io.load_wb
  wb_pipe.io.flush := io.flush

  // initialize value of valid-ready io
  load_queue_out.ready := false.B
  io.dcache_resp.ready := false.B

  io.dcache_req.valid := false.B
  io.dcache_req.bits := DontCare

  wb_pipe.io.in.valid := false.B
  wb_pipe.io.in.bits := DontCare

  io.mmio_commit.ready := false.B

  val sIdle :: sWaitDcacheResp :: sFlush :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val laod_req_buf = RegInit(0.U.asTypeOf(new LoadQueueIn))

  def send_dcache_req() = {
    when(load_queue_out.valid && !io.flush) {
      val isMmio = load_queue_out.bits.is_mmio
      io.mmio_commit.ready := isMmio
      when(isMmio && io.mmio_commit.fire || !isMmio) {
        io.dcache_req.valid := true.B
        io.dcache_req.bits.paddr := load_queue_out.bits.paddr
        io.dcache_req.bits.size := load_queue_out.bits.size
        io.dcache_req.bits.is_mmio := isMmio
        when(io.dcache_req.fire) {
          load_queue_out.ready := true.B
          assert(load_queue_out.fire, "load_queue_out should be fire")
          laod_req_buf := load_queue_out.bits
          state := sWaitDcacheResp
        }.otherwise {
          state := sIdle
        }
      }.otherwise {
        state := sIdle
      }
    }.otherwise {
      state := sIdle
    }
  }

  def compose_write_back_data(rdata: UInt, load_info: LoadQueueIn): UInt = {
    val rdata_vec = rdata.asTypeOf(Vec(8, UInt(8.W)))
    val wstrb_vec = load_info.store_bypass.wstrb.asBools
    val wdata_vec = load_info.store_bypass.wdata.asTypeOf(Vec(8, UInt(8.W)))

    require(
      rdata_vec.length == wstrb_vec.length && rdata_vec.length == wdata_vec.length,
      "rdata_vec,wstrb_vec,rdata_vec should have the same length"
    )
    when(load_info.store_bypass.valid) {
      for (i <- 0 until rdata_vec.length) {
        when(wstrb_vec(i)) {
          rdata_vec(i) := wdata_vec(i)
        }
      }
    }
    val right_aligned_sign_ext_rdata = GetAxiRdata(
      rdata_vec.asUInt,
      load_info.paddr,
      load_info.size,
      load_info.sign_ext
    )
    require(
      right_aligned_sign_ext_rdata.getWidth == rdata.getWidth,
      "width should be equal"
    )
    right_aligned_sign_ext_rdata
  }

//  def send_dcache_req_gpt4() = {
//    val load_queue_valid_and_not_flushing = load_queue_out.valid && !io.flush
//    val is_mmio = load_queue_out.bits.is_mmio
//    val mmio_commit_fired = is_mmio && io.mmio_commit.fire
//    val dcache_req_ready_to_fire =
//      io.dcache_req.ready && load_queue_valid_and_not_flushing && (mmio_commit_fired || !is_mmio)
//
//    io.mmio_commit.ready := is_mmio
//
//    io.dcache_req.valid := dcache_req_ready_to_fire
//    when(dcache_req_ready_to_fire) {
//      io.dcache_req.bits.paddr := load_queue_out.bits.paddr
//      io.dcache_req.bits.size := load_queue_out.bits.size
//      io.dcache_req.bits.is_mmio := is_mmio
//    }
//
//    when(dcache_req_ready_to_fire && io.dcache_req.fire) {
//      load_queue_out.ready := true.B
//      assert(load_queue_out.fire, "load_queue_out should be fire")
//      laod_req_buf := load_queue_out.bits
//      state := sWaitDcacheResp
//    }.otherwise {
//      state := sIdle
//    }
//  }

  switch(state) {

    is(sIdle) {
      send_dcache_req()
    }
    is(sWaitDcacheResp) {
      // back pressure
      io.dcache_resp.ready := wb_pipe.io.in.ready

      when(io.dcache_resp.fire && !io.flush) {
        // 1. flush is false, and dcache_resp is fire, receive the data
        wb_pipe.io.in.valid := true.B
        wb_pipe.io.in.bits.rdata := compose_write_back_data(
          io.dcache_resp.bits.data,
          laod_req_buf
        )
        wb_pipe.io.in.bits.tran_id := laod_req_buf.trans_id
        assert(wb_pipe.io.in.fire, "out_pipe.io.in.fire should be true.B")
        // back to back
        send_dcache_req()
      }.elsewhen(io.dcache_resp.fire && io.flush) {
        // 2. flush is true, and dcache_resp is fire, discard the data
        state := sIdle
      }.elsewhen(!io.dcache_resp.fire && io.flush) {
        // 3. flush is true, and dcache_resp is not fire, go to flush state to wait dcache_resp
        state := sFlush
      }.otherwise {
        // 4. flush is false, and dcache_resp is not fire, wait dcache_resp
        state := sWaitDcacheResp
      }
    }
    is(sFlush) {
      // if send a dcache_req, and do not receive a dcache_resp before flush.
      // wait dcache_resp, and discard the data
      io.dcache_resp.ready := true.B
      when(io.dcache_resp.fire) {
        state := sIdle
      }
    }
  }

  // ----------------------
  // assert
  // ----------------------
  assert(
    !(io.flush && io.mmio_commit.valid),
    "flush and mmio_commit should not be true at the same time"
  )

}

object gen_load_queue_verilog extends App {
  GenVerilogHelper(new LoadQueue)
}
