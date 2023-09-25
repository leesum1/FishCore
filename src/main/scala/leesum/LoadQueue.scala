package leesum
import chisel3._
import chisel3.util.{Decoupled, Enum, Queue, is, switch}
class LoadQueueIn extends Bundle {
  val paddr = UInt(64.W)
  // 0: 1 byte, 1: 2 bytes, 2: 4 bytes, 3: 8 bytes
  val size = UInt(2.W)
  val is_mmio = Bool()
  val trans_id = UInt(32.W)
}

class LoadWriteBack extends Bundle {
  val rdata = UInt(64.W)
  val tran_id = UInt(32.W)
}

class LoadQueue extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LoadQueueIn))
    val load_wb = Decoupled(new LoadWriteBack)
    // TODO: how to implement flush?
    val flush = Input(Bool())
    // from commit stage, when commit a mmio instruction, set mmio_commit to true
    val mmio_commit = Flipped(Decoupled())
    val dcache_req = Decoupled(new LoadDcacheReq)
    val dcache_resp = Flipped(Decoupled(new LoadDcacheResp))
  })
  val load_queue_size = 4
  val load_queue_out = Queue(
    io.in,
    load_queue_size,
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
  val tran_id_buf = RegInit(0.U(32.W))

  def sen_dcache_req(
      paddr: UInt,
      size: UInt,
      is_mmio: Bool,
      trans_id: UInt
  ): Unit = {
    io.dcache_req.valid := true.B
    io.dcache_req.bits.paddr := paddr
    io.dcache_req.bits.size := size
    io.dcache_req.bits.is_mmio := is_mmio
    when(io.dcache_req.fire) {
      assert(load_queue_out.valid, "load_queue_out should be valid")
      load_queue_out.ready := true.B
      tran_id_buf := trans_id
      state := sWaitDcacheResp
    }.otherwise {
      state := sIdle
    }
  }

  switch(state) {
    is(sIdle) {
      when(load_queue_out.valid && !io.flush) {
        val isMmio = load_queue_out.bits.is_mmio
        io.mmio_commit.ready := isMmio
        when(isMmio && io.mmio_commit.fire || !isMmio) {
          sen_dcache_req(
            load_queue_out.bits.paddr,
            load_queue_out.bits.size,
            isMmio,
            load_queue_out.bits.trans_id
          )
        }
      }
    }
    is(sWaitDcacheResp) {
      io.dcache_resp.ready := wb_pipe.io.in.ready
      // TODO: Improve me, back by back
      when(io.dcache_resp.fire && !io.flush) {
        // 1. flush is false, and dcache_resp is fire, receive the data
        wb_pipe.io.in.valid := true.B
        wb_pipe.io.in.bits.rdata := io.dcache_resp.bits.data
        wb_pipe.io.in.bits.tran_id := tran_id_buf

        when(wb_pipe.io.in.fire) {
          state := sIdle
        }.otherwise {
          assert(false.B, "out_pipe.io.in.fire should be true.B")
        }
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
