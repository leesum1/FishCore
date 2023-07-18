package leesum

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Cat

object RiscvTools {
  def is_rvc(inst: UInt): Bool = {
    inst(1, 0) =/= 3.U
  }

  def expand_rvc(inst: UInt): UInt = {
    inst
  }

  def get_inst(inst_h: UInt, inst_l: UInt): (Bool, UInt) = {
    val rvc = is_rvc(inst_l)

    val inst = Wire(UInt(32.W))
    when(rvc) {
      inst := expand_rvc(inst_l)
    }.otherwise {
      inst := Cat(inst_h(15,0), inst_l(15,0))
    }

    (rvc, inst)
  }
}
