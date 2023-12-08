package leesum.Utils

import chisel3._
import leesum.GenVerilogHelper

/** Implements the `OverrideWithMask` functionality.
  *
  * The `OverrideWithMask` object provides a method to override values in a
  * vector based on a mask. Only the values in the original vector that
  * correspond to a `1` in the mask will be replaced by the corresponding value
  * in the override vector.
  *
  * @note
  *   All input vectors must have the same size, otherwise an exception is
  *   thrown.
  */

object OverrideWithMask {
  def apply[T <: Data](pre: Vec[T], mask: UInt, value: Vec[T]): Vec[T] = {
    require(
      Seq(pre.size, mask.getWidth, value.size).distinct.size == 1,
      "All input must have same size"
    )
    val res = Wire(Vec(pre.size, pre(0).cloneType))
    for (i <- pre.indices) {
      res(i) := Mux(mask(i), value(i), pre(i))
    }
    res
  }
}

object gen_OverrideWithMask_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val pre = Input(Vec(4, UInt(8.W)))
      val mask = Input(UInt(4.W))
      val value = Input(Vec(4, UInt(8.W)))
      val res = Output(Vec(4, UInt(8.W)))
    })
    io.res := OverrideWithMask(io.pre, io.mask, io.value)
  })
}
