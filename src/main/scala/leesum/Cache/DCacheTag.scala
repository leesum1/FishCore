package leesum.Cache

import chisel3._
import chisel3.util.SRAM
import leesum.GenVerilogHelper
import leesum.Utils.HoldRegister

class DCacheTagBundle extends Bundle {

  val valid = Bool()
  val dirty = Bool()
  val tag = UInt(24.W)
}

class DCacheTag(tag_length: Int) extends ICacheTag(tag_length)

object gen_DCacheTag_verilog extends App {
  GenVerilogHelper(new DCacheTag(29))
}
