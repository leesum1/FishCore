package leesum

import chisel3._
import chisel3.util.{Mux1H, MuxCase, MuxLookup, PopCount, PriorityMux, Valid}
import chiseltest.ChiselScalatestTester
import chiseltest.formal.{BoundedCheck, Formal}
import org.scalatest.flatspec.AnyFlatSpec

/** This module is used to convert a InstsItem to a stream of INSTEntry
  * InstsItem is a bundle of 4 INSTEntry, and all InstEntry may be valid or not.
  * Such as [Valid, inValid, inValid, Valid], and this module will convert it to
  * [Valid, Valid, inValid,inValid] and keep the order of InstEntry. It remove
  * bubble and keep the order of InstEntry.
  */
class VecCompressor[T <: Data](gen: T, num: Int, formal: Boolean = false)
    extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(num, Valid(gen)))
    val out = Output(Vec(num, Valid(gen)))
  })

  require(num == 4, "only support 4 now")

  val valid_seq = VecInit(io.in.map(_.valid))
  val data_seq = VecInit(io.in.map(_.bits))
  val valid_count = PopCount(valid_seq)

  val out_data_0 = PriorityMux(valid_seq, data_seq)
  val out_data_3 = data_seq(3)
  val out_data_2 = Mux(
    valid_seq(0) & valid_seq(1),
    PriorityMux(valid_seq.drop(2), data_seq.drop(2)),
    data_seq(3)
  )

  val out_data_1_Pop2 =
    PriorityMux(
      valid_seq.reverse,
      data_seq.reverse
    ) // valid_seq PopCount == 2.U

  val out_data_1_Pop3 = MuxCase(
    data_seq(1),
    Seq(
      (!valid_seq(2) || !valid_seq(3)) -> data_seq(1),
      (!valid_seq(0) || !valid_seq(1)) -> data_seq(2)
    )
  ) // valid_seq PopCount == 3.U

  val out_data_1_Pop4 = data_seq(1) // valid_seq PopCount == 4.U

  val out_data_1 = Mux1H(
    Seq(
      (valid_count === 2.U) -> out_data_1_Pop2,
      (valid_count === 3.U) -> out_data_1_Pop3,
      (valid_count === 4.U) -> out_data_1_Pop4
    )
  )

  val compressed_valid = MuxLookup(valid_count, 0.U)(
    Seq(
      0.U -> 0.U,
      1.U -> 1.U,
      2.U -> 3.U,
      3.U -> 7.U,
      4.U -> 15.U
    )
  ).asBools
  require(compressed_valid.length == 4, "compressed_valid must be 4")

  val compressed_data = VecInit(out_data_0, out_data_1, out_data_2, out_data_3)
  io.out.zipWithIndex.foreach { case (x, i) =>
    x.valid := compressed_valid(i)
    x.bits := compressed_data(i)
  }

  // --------------------------
  // formal
  // --------------------------
  assert(PopCount(io.out.map(_.valid)) === PopCount(io.in.map(_.valid)))
  assert(CheckOrder(VecInit(io.out.map(_.valid))))
}
class VecCompressFormal
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Formal {
  "VecCompress" should "pass with assumption" in {
    verify(
      new VecCompressor(UInt(8.W), 4, formal = true),
      Seq(BoundedCheck(10))
    )
  }
}

object gen_VecCompressor_verilog extends App {
  GenVerilogHelper(new VecCompressor(UInt(32.W), 4))
}
