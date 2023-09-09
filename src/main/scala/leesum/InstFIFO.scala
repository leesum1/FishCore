package leesum

import chisel3._
import chisel3.util._

/** This module is used to convert a InstsItem to a stream of INSTEntry
  * InstsItem is a bundle of 4 INSTEntry, and all InstEntry may be valid or not.
  * Such as [Valid, inValid, inValid, Valid], and this module will convert it to
  * [Valid, Valid, inValid,inValid] and keep the order of InstEntry. It remove
  * bubble and keep the order of InstEntry.
  */
class CompressInstsItem extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new InstsItem))
    val out_valid = Output(Vec(4, Bool()))
    val out_data = Output(Vec(4, new INSTEntry))
  })

  val validSeq = VecInit(io.in.bits.insts_vec.map(_.valid))
  val validSeq_count = PopCount(validSeq)
  val dataSeq = io.in.bits.insts_vec

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

  val out_data_1_Pop4 = dataSeq(3) // validSeq PopCount == 4.U

  val out_data_1 = Mux1H(
    Seq(
      (validSeq_count === 2.U) -> out_data_1_Pop2,
      (validSeq_count === 3.U) -> out_data_1_Pop3,
      (validSeq_count === 4.U) -> out_data_1_Pop4
    )
  )

  io.out_data(0) := out_data_0
  io.out_data(1) := out_data_1
  io.out_data(2) := out_data_2
  io.out_data(3) := out_data_3

  val out_valid = Mux1H(
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

  io.out_valid := out_valid

  assert(CheckOrder(out_valid), "out_valid must be ordered")

  io.in.ready := true.B
}

object gen_InstFIFO_test extends App {
  GenVerilogHelper(new CompressInstsItem)
}
