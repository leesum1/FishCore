package leesum
import chisel3._
import chisel3.util.{Decoupled, Enum, Queue, is, switch}
class LoadQueueIn extends Bundle {
  val paddr = UInt(64.W)
  // 0: 1 byte, 1: 2 bytes, 2: 4 bytes, 3: 8 bytes
  val size = UInt(3.W)
  val is_mmio = Bool()
}

class LoadQueue extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LoadQueueIn))
    val out = Decoupled(UInt(64.W))
    val flush = Input(Bool())
    // from commit stage, when commit a mmio instruction, set mmio_commit to true
    val mmio_commit = Input(Bool())
    val dcache_req = Decoupled(new LoadDcacheReq)
    val dcache_resp = Flipped(Decoupled(new LoadDcacheResp))
  })
  val load_queue_size = 4
  val fifo_out = Queue(
    io.in,
    load_queue_size,
    flush = Some(io.flush)
  )

  fifo_out.ready := false.B
  io.dcache_req.valid := false.B
  io.dcache_req.bits.paddr := 0.U
  io.dcache_req.bits.size := 0.U
  io.dcache_req.bits.is_mmio := false.B
  io.dcache_resp.ready := false.B
  io.out.valid := false.B
  io.out.bits := 0.U

  val sIdle :: sWaitDcacheResp :: sFlush :: Nil = Enum(3)
  val state = RegInit(sIdle)

  def sen_dcache_req(paddr: UInt, size: UInt, is_mmio: Bool): Unit = {
    io.dcache_req.valid := true.B
    io.dcache_req.bits.paddr := paddr
    io.dcache_req.bits.size := size
    io.dcache_req.bits.is_mmio := is_mmio
    when(io.dcache_req.fire) {
      fifo_out.ready := true.B
      state := sWaitDcacheResp
    }.otherwise {
      state := sIdle
    }
  }

  switch(state) {
    is(sIdle) {
      when(fifo_out.valid && !io.flush) {
        val isMmio = fifo_out.bits.is_mmio
        when(isMmio && io.mmio_commit || !isMmio) {
          sen_dcache_req(fifo_out.bits.paddr, fifo_out.bits.size, isMmio)
        }
      }
    }
    is(sWaitDcacheResp) {
      io.dcache_resp.ready := true.B
      // TODO: Improve me
      when(io.dcache_resp.fire && !io.flush) {
        io.out.valid := true.B
        io.out.bits := io.dcache_resp.bits.data
        state := sIdle
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
