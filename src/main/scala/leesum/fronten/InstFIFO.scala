package leesum.fronten

import chisel3._
import chisel3.util._
import leesum.{BpEntry, DummyMultiPortFIFO, ExceptionEntry, GenVerilogHelper}

// TODO: exception
class INSTEntry extends Bundle {
  val pc = UInt(64.W)
  val inst = UInt(32.W)
  val inst_c = UInt(16.W)
  val rvc = Bool()
  val valid = Bool()
  val exception = new ExceptionEntry()
  val bp = new BpEntry()

  def clear() = {
    pc := 0.U
    inst := 0.U
    inst_c := 0.U
    exception.clear()
    bp.clear()
    rvc := false.B
    valid := false.B
  }
}

class InstsFIFO extends Module {
  val io = IO(new Bundle {
    val push = Flipped(Decoupled(Vec(4, new INSTEntry)))
    val pop = Vec(2, Decoupled(new INSTEntry))
    val flush = Input(Bool())
  })

  val inst_fifo = Module(
    new DummyMultiPortFIFO(
      gen = new INSTEntry,
      size = 16,
      num_push_ports = 4,
      num_pop_ports = 2
    )
  )
  val push_cond = io.push.bits.map(_.valid && io.push.fire)
  io.push.ready := inst_fifo.io.in.map(_.ready).reduce(_ && _)
  for (i <- 0 until 4) {
    inst_fifo.io.in(i).valid := push_cond(i)
    inst_fifo.io.in(i).bits := io.push.bits(i)
  }

  inst_fifo.io.out <> io.pop
  inst_fifo.io.flush := io.flush

//  val inst_fifo = new MultiPortFIFOBase(
//    gen = new INSTEntry,
//    size = 16,
//    num_push_ports = 4,
//    num_pop_ports = 2,
//    use_mem = false,
//    with_valid = false
//  )
//  val push_cond = io.push.bits.map(_.valid && io.push.fire)
//  inst_fifo.push_pop_flush_cond(
//    push_cond = push_cond,
//    pop_cond = io.pop.map(_.fire),
//    flush_cond = io.flush,
//    entry = io.push.bits
//  )
//
//  io.push.ready := inst_fifo.free_entries >= 4.U
//
//  val fifo_peek = inst_fifo.peek()
//  fifo_peek.zip(io.pop).foreach { case (peek, io_pop) =>
//    io_pop.bits := peek.bits
//    io_pop.valid := peek.valid
//  }
}

object gen_InstFIFO_test2 extends App {
  GenVerilogHelper(new InstsFIFO)
}
