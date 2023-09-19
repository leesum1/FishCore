package leesum
import chisel3._
import chisel3.util.{Decoupled, Enum, MuxLookup, Queue, is, switch}
import circt.stage.ChiselStage

class PipeLine[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(gen))
    val out = Decoupled(UInt(64.W))
    val flush = Input(Bool())
  })

  val pipe_fifo = Queue(io.in, 1, pipe = true, flush = Some(io.flush))

  io.out <> pipe_fifo
}

object gen_pipeline_verilog extends App {
  GenVerilogHelper(new PipeLine(UInt(64.W)))
}
