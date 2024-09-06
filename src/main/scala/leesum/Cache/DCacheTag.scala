package leesum.Cache

import chisel3._
import leesum.GenVerilogHelper

class DCacheTagBundle extends Bundle {

  val valid = Bool()
  val dirty = Bool()
  val tag = UInt(24.W)
}

class DCacheTag(tag_length: Int) extends ICacheTag(tag_length)

object gen_DCacheTag_verilog extends App {
  GenVerilogHelper(new DCacheTag(29))
}
