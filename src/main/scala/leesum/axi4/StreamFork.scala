package leesum.axi4

import chisel3._
import chisel3.util._
import leesum.GenVerilogHelper

/** This module forks a stream into multiple streams.For each input stream
  * handshake, every output stream handshakes exactly once. The input stream
  * only handshakes when all output streams have handshake, but the output
  * streams do not have to handshake simultaneously.
  *
  * @param gen
  * @param size
  * @param synchronous
  * @tparam T
  */
class StreamFork[T <: Data](
    gen: T,
    size: Int,
    synchronous: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(gen))
    val outputs = Vec(size, Decoupled(gen))
  })

  val portCount = io.outputs.size
  /* Used for async, Store if an output stream already has taken its value or not */
  val linkEnable =
    if (!synchronous) RegInit(VecInit(Seq.fill(portCount)(true.B))) else null

  if (synchronous) {
    io.input.ready := io.outputs.map(_.ready).reduce(_ && _)
    io.outputs.foreach(_.valid := io.input.valid && io.input.ready)
    io.outputs.foreach(_.bits := io.input.bits)
  } else {
    /* Ready is true when every output stream takes or has taken its value */
    io.input.ready := true.B
    for (i <- 0 until portCount) {
      when(!io.outputs(i).ready && linkEnable(i)) {
        io.input.ready := false.B
      }
    }

    /* Outputs are valid if the input is valid and they haven't taken their value yet.
     * When an output fires, mark its value as taken. */
    for (i <- 0 until portCount) {
      io.outputs(i).valid := io.input.valid && linkEnable(i)
      io.outputs(i).bits := io.input.bits
      when(io.outputs(i).fire) {
        linkEnable(i) := false.B
      }
    }

    /* Reset the storage for each new value */
    when(io.input.ready) {
      linkEnable.foreach(_ := true.B)
    }
  }
}

object StreamFork {
  def apply[T <: Data](
      in: DecoupledIO[T],
      size: Int,
      synchronous: Boolean = false
  ): Seq[DecoupledIO[T]] = {
    val fork = Module(new StreamFork(in.bits.cloneType, size, synchronous))
    fork.io.input <> in
    fork.io.outputs
  }
}

object StreamFork2 {
  def apply[T <: Data](
      in: DecoupledIO[T],
      synchronous: Boolean = false
  ): (DecoupledIO[T], DecoupledIO[T]) = {
    val fork = Module(new StreamFork(in.bits.cloneType, 2, synchronous))
    fork.io.input <> in
    (fork.io.outputs(0), fork.io.outputs(1))
  }
}

object StreamFork3 {
  def apply[T <: Data](
      in: DecoupledIO[T],
      synchronous: Boolean = false
  ): (DecoupledIO[T], DecoupledIO[T], DecoupledIO[T]) = {
    val fork = Module(new StreamFork(in.bits.cloneType, 3, synchronous))
    fork.io.input <> in
    (fork.io.outputs(0), fork.io.outputs(1), fork.io.outputs(2))
  }
}

object gen_stream_fork_verilog extends App {
  GenVerilogHelper(
    new StreamFork(UInt(64.W), 2, synchronous = false)
  )

}
