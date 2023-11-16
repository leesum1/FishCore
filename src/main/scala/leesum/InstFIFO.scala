package leesum

import chisel3._
import chisel3.util._

// TODO: exception
class INSTEntry extends Bundle {
  val pc = UInt(64.W)
  val inst = UInt(32.W)
  val inst_c = UInt(16.W)
  val rvc = Bool()
  val valid = Bool()
  val exception = new ExceptionEntry()

  def clear() = {
    pc := 0.U
    inst := 0.U
    inst_c := 0.U
    exception.clear()
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

  val vec_compress = Module(new VecCompressor(new INSTEntry, 4))
  vec_compress.io.in.zipWithIndex.foreach {
    case (x, i) => {
      x.bits := io.push.bits(i)
      x.valid := io.push.valid && io.push.bits(i).valid
    }
  }

  val inst_fifo = new MultiPortValidFIFO(
    gen = new INSTEntry,
    size = 16,
    name = "inst_fifo",
    num_push_ports = 4,
    num_pop_ports = 2
  )
  val push_cond = vec_compress.io.out.map(_.valid & io.push.fire)
  inst_fifo.push_pop_flush_cond_multi_port(
    push_cond = VecInit(push_cond),
    pop_cond = VecInit(io.pop.map(_.fire)),
    flush_cond = io.flush,
    entry = VecInit(vec_compress.io.out.map(_.bits))
  )

  io.push.ready := inst_fifo.free_entries >= 4.U

  val fifo_peek = inst_fifo.peek()
  fifo_peek.zip(io.pop).foreach { case (peek, io_pop) =>
    io_pop.bits := peek.bits
    io_pop.valid := peek.valid
  }
}

object gen_InstFIFO_test2 extends App {
  GenVerilogHelper(new InstsFIFO)
}
