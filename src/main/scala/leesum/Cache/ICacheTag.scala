package leesum.Cache

import chisel3._
import chisel3.util.SRAM
import leesum.GenVerilogHelper

class ICacheTagBundle extends Bundle {

  val valid = Bool()
  val tag = UInt(24.W)
}

class ICacheTag extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(6.W)) // max 64
    val rdata = Output(UInt(24.W))
    val tag_valid = Output(Bool())
    val en = Input(Bool())
    val wen = Input(Bool())
    val wdata = Input(UInt(24.W))
    val flush = Input(Bool())
  })

  val valid_reg = RegInit(VecInit(Seq.fill(64)(false.B)))

  val tag = SRAM(64, UInt(24.W), 0, 0, 1)

  tag.readwritePorts(0).address := io.addr
  tag.readwritePorts(0).writeData := io.wdata
  tag.readwritePorts(0).isWrite := io.wen
  tag.readwritePorts(0).enable := io.en
  io.rdata := tag.readwritePorts(0).readData

  // keep the last read data
  val last_rdata = RegInit(0.U(24.W))
  val last_rvalid = RegInit(false.B)
  // current operation is read
  val read_op = io.en && !io.wen

  // keep the last read data when current operation is read
  when(RegNext(read_op)) {
    last_rdata := tag.readwritePorts(0).readData
  }

  when(read_op) {
    last_rvalid := valid_reg(io.addr)
  }

  io.rdata := Mux(RegNext(read_op), tag.readwritePorts(0).readData, last_rdata)
  io.tag_valid := last_rvalid

  when(io.wen) {
    valid_reg(io.addr) := true.B
  }

  when(io.flush) {
    valid_reg := VecInit(Seq.fill(64)(false.B))
  }
}

object gen_ICacheTag_verilog extends App {
  GenVerilogHelper(new ICacheTag)
}
