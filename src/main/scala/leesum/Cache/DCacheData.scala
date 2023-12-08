package leesum.Cache

import chisel3._
import leesum.GenVerilogHelper
import leesum.Utils.HoldRegister

class DCacheData extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(6.W)) // max 64
    val rdata = Output(UInt(128.W))
    val en = Input(Bool())
    val wen = Input(Bool())
    val wdata = Input(UInt(128.W))
    val wmask = Input(UInt(16.W))
  })

  val sram = Module(new SramTemplateMask)
  sram.io.addr := io.addr
  sram.io.en := io.en
  sram.io.wen := io.wen
  sram.io.wdata := io.wdata
  sram.io.wmask := io.wmask

  // keep the last read data
  val read_op = io.en && !io.wen
  io.rdata := HoldRegister(read_op, sram.io.rdata, 1)
}

object gen_DCacheData_verilog extends App {
  GenVerilogHelper(new DCacheData)
}
