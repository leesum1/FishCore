package leesum
import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Enum, MuxLookup, Queue, is, switch}

class PipeLine[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(gen))
    val out = Decoupled(gen)
    val flush = Input(Bool())
  })

  val pipe_fifo = Queue(io.in, 1, pipe = true, flush = Some(io.flush))
  io.out <> pipe_fifo
}

object PipeLine {
  def apply[T <: Data](in: DecoupledIO[T], flush: Bool): DecoupledIO[T] = {
    val pipe = Module(new PipeLine(in.bits.cloneType))
    pipe.io.in <> in
    pipe.io.flush := flush
    pipe.io.out
  }
}

object gen_pipeline_verilog extends App {
  GenVerilogHelper(new PipeLine(UInt(64.W)))
}
