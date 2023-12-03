package leesum.Cache

import chisel3._
import chisel3.util.SRAM
import leesum.GenVerilogHelper
import leesum.Utils.HoldRegister

class ICacheData extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(6.W)) // max 64
    val rdata = Output(UInt(128.W))
    val en = Input(Bool())
    val wen = Input(Bool())
    val wdata = Input(UInt(128.W))
  })

  val sram = Module(new SramTemplate)
  sram.io.addr := io.addr
  sram.io.en := io.en
  sram.io.wen := io.wen
  sram.io.wdata := io.wdata

  // keep the last read data
  val read_op = io.en && !io.wen
  io.rdata := HoldRegister(read_op, sram.io.rdata, 1)
}

class SramTemplateMask extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(6.W)) // max 64
    val rdata = Output(UInt(128.W))
    val en = Input(Bool())
    val wen = Input(Bool())
    val wmask = Input(UInt(16.W))
    val wdata = Input(UInt(128.W))
  })
  val sram = SRAM.masked(64, Vec(16, UInt(8.W)), 0, 0, 1)

  sram.readwritePorts(0).address := io.addr
  sram.readwritePorts(0).writeData := io.wdata.asTypeOf(Vec(16, UInt(8.W)))
  sram.readwritePorts(0).isWrite := io.wen
  sram.readwritePorts(0).enable := io.en
  sram.readwritePorts(0).mask.get := VecInit(io.wmask.asBools)
  io.rdata := sram.readwritePorts(0).readData.asUInt
}

class SramTemplate extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(6.W)) // max 64
    val rdata = Output(UInt(128.W))
    val en = Input(Bool())
    val wen = Input(Bool())
    val wdata = Input(UInt(128.W))
  })
  val sram = SRAM(64, UInt(128.W), 0, 0, 1)

  sram.readwritePorts(0).address := io.addr
  sram.readwritePorts(0).writeData := io.wdata
  sram.readwritePorts(0).isWrite := io.wen
  sram.readwritePorts(0).enable := io.en
//  sram.readwritePorts(0).mask.get := VecInit(io.wmask.asBools)
  io.rdata := sram.readwritePorts(0).readData
}

object gen_ICacheData_verilog extends App {
  GenVerilogHelper(new ICacheData)
}
