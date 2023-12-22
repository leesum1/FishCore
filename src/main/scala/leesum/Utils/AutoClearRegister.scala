package leesum.Utils

import chisel3._
import leesum.GenVerilogHelper
object AutoClearRegister {
  def apply[T <: Data](in_valid: Bool, in: T, init: T): T = {
    val autoclear_reg = RegInit(init)

    when(!(autoclear_reg === init)) {
      autoclear_reg := init
    }

    when(in_valid) {
      autoclear_reg := in
    }
    autoclear_reg
  }
}

object gen_autoclear_register_verilog extends App {
  GenVerilogHelper(
    new Module {
      val io = IO(new Bundle {
        val in_valid = Input(Bool())
        val in = Input(UInt(64.W))
        val out = Output(UInt(64.W))
      })
      io.out := AutoClearRegister(io.in_valid, io.in, 1.U)
    }
  )
}
