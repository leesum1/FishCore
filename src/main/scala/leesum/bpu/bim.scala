package leesum.bpu

import chisel3._
import chisel3.util.{isPow2, log2Ceil}
import leesum.GenVerilogHelper
import leesum.Utils.{DualPortSRAM, HoldRegister}

class BIM(nums: Int) extends Module {
  val io = IO(new Bundle {
    // lookup
    val lookup_en = Input(Bool())
    val lookup_pc = Input(UInt(39.W)) // sv39
    val bim_value = Output(UInt(2.W))
    val bim_valid = Output(Bool())
    // refill
    val refill_en = Input(Bool())
    val refill_pc = Input(UInt(39.W))
    val refill_data = Input(UInt(2.W))
    // clear
    val clear_en = Input(Bool())
  })

  require(isPow2(nums) && nums > 1, "nums should be power of 2")

  val bim_valids = RegInit(VecInit(Seq.fill(nums)(false.B)))
  val bim_datas = Module(new DualPortSRAM(nums, UInt(2.W)))

  bim_datas.io.raddr := io.lookup_pc(3 + log2Ceil(nums), 3)
  bim_datas.io.ren := io.lookup_en
  bim_datas.io.waddr := io.refill_pc(3 + log2Ceil(nums), 3)
  bim_datas.io.wen := io.refill_en
  bim_datas.io.wdata := io.refill_data

  val bim_rdata = bim_datas.io.rdata
  val bim_rdata_valid = RegNext(bim_valids(io.lookup_pc(2 + log2Ceil(nums), 3)))

  io.bim_value := bim_rdata
  io.bim_valid := HoldRegister(io.lookup_en, bim_rdata_valid, 1)

  when(io.refill_en) {
    bim_valids(io.refill_pc(2 + log2Ceil(nums), 3)) := true.B
  }

  when(io.clear_en) {
    bim_valids.foreach(_ := false.B)
  }

}

object gen_bim_verilog extends App {
  GenVerilogHelper(new BIM(64))
}
