package leesum.Cache

import chisel3._
import chisel3.util.SRAM
import leesum.GenVerilogHelper
import leesum.Utils.HoldRegister

class ICacheTag(tag_length: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(6.W)) // max 64
    val rdata = Output(UInt(tag_length.W))
    val tag_valid = Output(Bool())
    val en = Input(Bool())
    val wen = Input(Bool())
    val wdata = Input(UInt(tag_length.W))
    val flush = Input(Bool())
  })

  val valid_reg = RegInit(VecInit(Seq.fill(64)(false.B)))

  val tag = SRAM(64, UInt(tag_length.W), 0, 0, 1)

  tag.readwritePorts(0).address := io.addr
  tag.readwritePorts(0).writeData := io.wdata
  tag.readwritePorts(0).isWrite := io.wen
  tag.readwritePorts(0).enable := io.en

  // keep the last read data when current operation is read
  val read_op = io.en && !io.wen
  io.rdata := HoldRegister(read_op, tag.readwritePorts(0).readData, 1)
  io.tag_valid := HoldRegister(read_op, RegNext(valid_reg(io.addr)), 1)

  when(io.wen) {
    valid_reg(io.addr) := true.B
  }

  when(io.flush) {
    valid_reg := VecInit(Seq.fill(64)(false.B))
  }
}

object gen_ICacheTag_verilog extends App {
  GenVerilogHelper(new ICacheTag(27))
}
