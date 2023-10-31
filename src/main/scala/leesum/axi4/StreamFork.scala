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
class StreamFork[T <: Data](gen: T, size: Int, synchronous: Boolean = false)
    extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(gen))
    val outputs = Vec(size, Decoupled(gen))
  })

  val portCount = io.outputs.size

  if (synchronous) {
    // In synchronous mode, all outputs are valid when the input is ready and valid.
    io.input.ready := io.outputs.map(_.ready).reduce(_ && _)
    io.outputs.foreach { o =>
      o.valid := io.input.valid && io.input.ready
      o.bits := io.input.bits
    }
  } else {
    // In asynchronous mode, use a register to track which outputs have accepted the input.
    val linkEnable = RegInit(VecInit(Seq.fill(portCount)(true.B)))

    // The input is ready if all outputs are ready or have accepted the input.
    io.input.ready := linkEnable
      .zip(io.outputs)
      .map { case (enable, output) =>
        output.ready || !enable
      }
      .reduce(_ && _)

    // Outputs are valid if the input is valid and they haven't taken their value yet.
    // When an output fires, mark its value as taken.
    io.outputs.zip(linkEnable).foreach { case (output, enable) =>
      output.valid := io.input.valid && enable
      output.bits := io.input.bits
      when(output.fire) {
        enable := false.B
      }
    }

    // Reset the storage for each new value when the input is ready.
    when(io.input.fire) {
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
