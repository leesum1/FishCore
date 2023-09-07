// See README.md for license details.

package gcd

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Queue
import chisel3.util.Irrevocable

/** Compute GCD using subtraction method. Subtracts the smaller from the larger
  * until register y is zero. value in register x is then the GCD
  */
class GCD extends Module {
  val io = IO(new Bundle {
    val value1 = Input(UInt(16.W))
    val value2 = Input(UInt(16.W))
    val loadingValues = Input(Bool())
    val outputGCD = Output(UInt(16.W))
    val outputValid = Output(Bool())
  })

  val x = Reg(UInt())
  val y = Reg(UInt())

  when(x > y) { x := x - y }
    .otherwise { y := y - x }

  when(io.loadingValues) {
    x := io.value1
    y := io.value2
  }

  io.outputGCD := x
  io.outputValid := y === 0.U
}

class my_stream_fifo extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Irrevocable(UInt(8.W)))
    val out = Irrevocable(UInt(8.W))
  })

  val fifo = new Queue(UInt(8.W), 4)

  fifo.io.enq <> io.in
  io.out <> fifo.io.deq
}

object my_stream_fifo extends App {
  emitVerilog(new my_stream_fifo())
}

class Queue5(w: Int, n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Irrevocable(UInt(w.W)))
    val out = Irrevocable(UInt(w.W))
  })

  val x = Queue.irrevocable(io.in, n)

  x.ready := io.out.ready
  io.out.valid := x.valid
  io.out.bits := x.bits
}

object my_stream_fifo1 extends App {
  emitVerilog(new Queue5(8, 8))
}
