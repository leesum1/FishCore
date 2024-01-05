package leesum.Utils

import chisel3._
import chisel3.util.{SRAM, isPow2, log2Ceil}
import leesum.GenVerilogHelper

class SinglePortSRAM[T <: Data](nums: Int, tpe: T) extends Module {
  require(isPow2(nums) && nums > 1, "nums should be power of 2")
  val addr_width = log2Ceil(nums)
  val io = IO(new Bundle {
    val addr = Input(UInt(addr_width.W))
    val rdata = Output(tpe)
    val en = Input(Bool())
    val wen = Input(Bool())
    val wdata = Input(tpe)
  })

  val sram_one_port = SRAM(nums, tpe, 0, 0, 1)

  sram_one_port.readwritePorts(0).address := io.addr
  sram_one_port.readwritePorts(0).writeData := io.wdata
  sram_one_port.readwritePorts(0).isWrite := io.wen
  sram_one_port.readwritePorts(0).enable := io.en

  // keep the last read data when current operation is read
  val read_op = io.en && !io.wen
  io.rdata := HoldRegister(read_op, sram_one_port.readwritePorts(0).readData, 1)
}

object gen_SinglePortSRAM_verilog extends App {
  GenVerilogHelper(new SinglePortSRAM(64, UInt(64.W)))
}
