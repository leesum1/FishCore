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

class DualPortSRAM[T <: Data](nums: Int, tpe: T) extends Module {
  require(isPow2(nums) && nums > 1, "nums should be power of 2")
  val addr_width = log2Ceil(nums)
  val io = IO(new Bundle {
    // read
    val raddr = Input(UInt(addr_width.W))
    val ren = Input(Bool())
    val rdata = Output(tpe)
    // write
    val waddr = Input(UInt(addr_width.W))
    val wen = Input(Bool())
    val wdata = Input(tpe)
  })

  val sram_dual_port = SRAM(nums, tpe, 1, 1, 0)

  sram_dual_port.readPorts(0).address := io.raddr
  sram_dual_port.readPorts(0).enable := io.ren

  sram_dual_port.writePorts(0).address := io.waddr
  sram_dual_port.writePorts(0).data := io.wdata
  sram_dual_port.writePorts(0).enable := io.wen

  // keep the last read data when current operation is read
  val read_op = io.ren
  io.rdata := HoldRegister(
    read_op,
    sram_dual_port.readPorts(0).data,
    1
  )
}

object gen_SinglePortSRAM_verilog extends App {
  GenVerilogHelper(new SinglePortSRAM(64, UInt(64.W)))
}

object gen_DualPortSRAM_verilog extends App {
  GenVerilogHelper(new DualPortSRAM(64, UInt(64.W)))
}
