package leesum.dbg

import chisel3._
import chisel3.util.{Cat, Mux1H, MuxLookup, log2Ceil}
import leesum.GenVerilogHelper

class JtagShiftReg(max_width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val shift_in = Input(Bool())
    val shift_out = Output(Bool())
    val update_valid = Input(Bool())
    val update_data = Input(UInt(max_width.W))
    val update_data_len = Input(UInt(log2Ceil(max_width + 1).W))

    val shift_valid = Input(Bool())
    val shift_reg_out = Output(UInt(max_width.W))
  })

  val shift_reg = RegInit(0.U(max_width.W))
  val shift_reg_len = RegInit(0.U(log2Ceil(max_width + 1).W))
  val shift_out_buf = RegInit(false.B)
  when(io.update_valid) {
    shift_reg := io.update_data
    shift_reg_len := io.update_data_len
  }.elsewhen(io.shift_valid) {
    shift_out_buf := shift_reg(0)
    val next_shift_reg_uselookup = MuxLookup(
      shift_reg_len,
      shift_reg
    ) {
      (0 to max_width)
        .map(i => {
          if (i < 2) {
            i.U -> Cat(0.U((max_width - 1).W), io.shift_in)
          } else if (i == max_width) {
            i.U -> Cat(io.shift_in, shift_reg(i - 1, 1))
          } else {
            i.U -> Cat(
              0.U((max_width - i - 1).W),
              io.shift_in,
              shift_reg(i - 1, 1)
            )
          }

        })
    }
    val next_shift_reg_use1H = Mux1H(
      (0 to max_width)
        .map(i => {
          if (i < 2) {
            (shift_reg_len === i.U) -> Cat(0.U((max_width - 1).W), io.shift_in)

          } else if (i == max_width) {
            (shift_reg_len === i.U) -> Cat(io.shift_in, shift_reg(i - 1, 1))
          } else {
            (shift_reg_len === i.U) -> Cat(
              0.U((max_width - i - 1).W),
              io.shift_in,
              shift_reg(i - 1, 1)
            )
          }
        })
    )
    shift_reg := next_shift_reg_use1H
  }

  io.shift_out := shift_out_buf
  io.shift_reg_out := shift_reg

  when(io.update_valid) {
    assert(
      io.update_data_len <= max_width.U && io.update_data_len > 0.U,
      "Invalid update_data_len"
    )
  }
}

object GenJtagShiftRegVerilog extends App {
  GenVerilogHelper(
    new JtagShiftReg(32)
  )
}
