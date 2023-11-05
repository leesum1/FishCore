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

  def clear() = {
    pc := 0.U
    inst := 0.U
    inst_c := 0.U
    rvc := false.B
    valid := false.B
  }
}

/** This module is used to convert a InstsItem to a stream of INSTEntry
  * InstsItem is a bundle of 4 INSTEntry, and all InstEntry may be valid or not.
  * Such as [Valid, inValid, inValid, Valid], and this module will convert it to
  * [Valid, Valid, inValid,inValid] and keep the order of InstEntry. It remove
  * bubble and keep the order of InstEntry.
  */
class CompressInstsItem extends Module {
  val io = IO(new Bundle {
    val in = Input(new InstsItem)
    val out = Output(new InstsItem)
  })

  val validSeq = VecInit(io.in.insts_vec.map(_.valid))
  val validSeq_count = PopCount(validSeq)
  val dataSeq = io.in.insts_vec

  val out_data_0 = PriorityMux(validSeq, dataSeq)
  val out_data_3 = dataSeq(3)
  val out_data_2 = Mux(
    validSeq(0) & validSeq(1),
    PriorityMux(validSeq.drop(2), dataSeq.drop(2)),
    dataSeq(3)
  )

  val out_data_1_Pop2 =
    PriorityMux(validSeq.reverse, dataSeq.reverse) // validSeq PopCount == 2.U

  val out_data_1_Pop3 = MuxCase(
    dataSeq(1),
    Seq(
      (!validSeq(2) || !validSeq(3)) -> dataSeq(1),
      (!validSeq(0) || !validSeq(1)) -> dataSeq(2)
    )
  ) // validSeq PopCount == 3.U

  val out_data_1_Pop4 = dataSeq(1) // validSeq PopCount == 4.U

  val out_data_1 = Mux1H(
    Seq(
      (validSeq_count === 2.U) -> out_data_1_Pop2,
      (validSeq_count === 3.U) -> out_data_1_Pop3,
      (validSeq_count === 4.U) -> out_data_1_Pop4
    )
  )

  val out_valid_seq = Mux1H(
    Seq(
      (validSeq_count === 0.U) -> VecInit(
        false.B,
        false.B,
        false.B,
        false.B
      ),
      (validSeq_count === 1.U) -> VecInit(
        true.B,
        false.B,
        false.B,
        false.B
      ),
      (validSeq_count === 2.U) -> VecInit(true.B, true.B, false.B, false.B),
      (validSeq_count === 3.U) -> VecInit(true.B, true.B, true.B, false.B),
      (validSeq_count === 4.U) -> VecInit(true.B, true.B, true.B, true.B)
    )
  )
  val out_data_seq = Seq(out_data_0, out_data_1, out_data_2, out_data_3)

  for (i <- 0 until 4) {
    io.out.insts_vec(i).valid := out_valid_seq(i)
    io.out.insts_vec(i).inst := out_data_seq(i).inst
    io.out.insts_vec(i).rvc := out_data_seq(i).rvc
    io.out.insts_vec(i).pc := out_data_seq(i).pc
    io.out.insts_vec(i).inst_c := out_data_seq(i).inst_c
  }

  assert(CheckOrder(out_valid_seq), "out_valid must be ordered")

}

class InstsFIFO extends Module {
  val io = IO(new Bundle {
    val push = Flipped(Decoupled(new InstsItem))
    val pop = Vec(2, Decoupled(new INSTEntry))
    val flush = Input(Bool())
  })

  val compress = Module(new CompressInstsItem)
  compress.io.in := io.push.bits

  val inst_fifo = new MultiPortValidFIFO(
    gen = new INSTEntry,
    size = 16,
    name = "inst_fifo",
    num_push_ports = 4,
    num_pop_ports = 2
  )
  val push_cond = compress.io.out.insts_vec.map(_.valid & io.push.fire)
  inst_fifo.push_pop_flush_cond_multi_port(
    push_cond = VecInit(push_cond),
    pop_cond = VecInit(io.pop.map(_.fire)),
    flush_cond = io.flush,
    entry = compress.io.out.insts_vec
  )

  io.push.ready := inst_fifo.free_entries >= 4.U

  val fifo_peek = inst_fifo.peek()
  fifo_peek.zip(io.pop).foreach { case (peek, io_pop) =>
    io_pop.bits := peek.bits
    io_pop.valid := peek.valid
  }

}

object gen_InstFIFO_test extends App {
  GenVerilogHelper(new CompressInstsItem)
}

object gen_InstFIFO_test2 extends App {
  GenVerilogHelper(new InstsFIFO)
}
