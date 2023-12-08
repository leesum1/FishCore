package leesum.Cache

import chisel3._
import chisel3.util.{PopCount, SRAM}
import leesum.GenVerilogHelper
import leesum.Utils.HoldRegister

class DCacheDirty extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(6.W)) // max 64
    val rdata = Output(Bool())
    val wdata = Input(Bool())
    val wen = Input(Bool())
    val en = Input(Bool())
  })
  val read_op = io.en && !io.wen
  val write_op = io.en && io.wen

  val dirty_sram = SRAM(64, Bool(), 0, 0, 1)

  dirty_sram.readwritePorts(0).address := io.addr
  dirty_sram.readwritePorts(0).writeData := io.wdata
  dirty_sram.readwritePorts(0).isWrite := io.wen
  dirty_sram.readwritePorts(0).enable := io.en
  io.rdata := HoldRegister(read_op, dirty_sram.readwritePorts(0).readData, 1)

  // --------------------
  // assert
  // --------------------
  assert(
    PopCount(Seq(read_op, write_op)) <= 1.U,
    "read_op and write_op should be mutually exclusive"
  )
}

object gen_DCacheDirty_verilog extends App {
  GenVerilogHelper(new DCacheDirty)
}
