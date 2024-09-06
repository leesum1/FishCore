package leesum.Utils

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.{TruthTable, decoder}

// TODO: UNTESTED
object DecoderHelper {

//  private def to_bitpat(
//      vec: List[UInt]
//  ) = {
//    vec.map(BitPat(_)).reduce((_: BitPat) ## (_: BitPat))
//  }

  def gen[T <: Data](
      input: UInt,
      default: T,
      mapping: Seq[(UInt, T)]
  ): T = {
    apply(input, default, mapping.map(x => (BitPat(x._1), x._2)))
  }

  def apply[T <: Data](
      input: UInt,
      default: T,
      mapping: Seq[(BitPat, T)]
  ): T = {

    mapping.foreach(x =>
      println(s"DecoderTree: ${x._1.getWidth} -> ${x._2.getWidth}")
    )
    require(
      mapping.map(_._1.getWidth).distinct.size == 1,
      "All BitPat must have same width"
    )
    require(
      mapping.map(_._2.getWidth).distinct.size == 1,
      "All output must have same width"
    )
    require(input.getWidth == mapping(0)._1.getWidth, "Input width mismatch")
    require(default.getWidth == mapping(0)._2.getWidth, "Output width mismatch")

    val table = mapping.map(x => (x._1, BitPat(x._2.asUInt))).toList
    val true_table = TruthTable(table, BitPat(default.asUInt))
    val res = decoder(input, true_table)
    res.asTypeOf(default.cloneType)
  }

}
