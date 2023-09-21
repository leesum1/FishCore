package leesum
import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Enum, Queue, is, switch}
class LoadQueueIn extends Bundle {
  val paddr = UInt(64.W)
  // 0: 1 byte, 1: 2 bytes, 2: 4 bytes, 3: 8 bytes
  val size = UInt(2.W)
  val is_mmio = Bool()
}

class LoadQueue extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LoadQueueIn))
    val out = Decoupled(UInt(64.W))
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
  val out_pipe = Module(new PipeLine(UInt(64.W)))
  out_pipe.io.out <> io.out
  out_pipe.io.flush := io.flush

  load_queue_out.ready := false.B
  io.dcache_req.valid := false.B
  io.dcache_req.bits.paddr := 0.U
  io.dcache_req.bits.size := 0.U
  io.dcache_req.bits.is_mmio := false.B
  io.dcache_resp.ready := false.B
  out_pipe.io.in.valid := false.B
  out_pipe.io.in.bits := DontCare

  io.mmio_commit.ready := false.B

  val sIdle :: sWaitDcacheResp :: sFlush :: Nil = Enum(3)
  val state = RegInit(sIdle)

  def sen_dcache_req(paddr: UInt, size: UInt, is_mmio: Bool): Unit = {
    io.dcache_req.valid := true.B
    io.dcache_req.bits.paddr := paddr
    io.dcache_req.bits.size := size
    io.dcache_req.bits.is_mmio := is_mmio
    when(io.dcache_req.fire) {
      load_queue_out.ready := true.B
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
            isMmio
          )
        }
      }
    }
    is(sWaitDcacheResp) {
      io.dcache_resp.ready := out_pipe.io.in.ready
      // TODO: Improve me, back by back
      when(io.dcache_resp.fire && !io.flush) {
        out_pipe.io.in.valid := true.B
        out_pipe.io.in.bits := io.dcache_resp.bits.data
        when(out_pipe.io.in.fire) {
          state := sIdle
        }.otherwise {
          assert(false.B, "out_pipe.io.in.fire should be true.B")
        }
      }.elsewhen(io.dcache_resp.fire && io.flush) {
        state := sIdle
      }.elsewhen(!io.dcache_resp.fire && io.flush) {
        state := sFlush
      }
    }
    is(sFlush) {
      io.dcache_resp.ready := true.B
      when(io.dcache_resp.fire) {
        state := sIdle
      }
    }
  }
}

object gen_load_queue_verilog extends App {
  GenVerilogHelper(new LoadQueue)
}
