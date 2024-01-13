package leesum.bpu

import chisel3._
import chisel3.util.{isPow2, log2Ceil}
import leesum.Utils.HoldRegister
import leesum.{BpType, GenVerilogHelper}

class BPInfo extends Bundle {

  val valid = Bool()
  val target_pc = UInt(39.W)
  val branch_offset = UInt(3.W) // the first jump instruction offset
  val bim_value = UInt(2.W) // bim value, used to distinguish taken or not
  val is_rvc = Bool()
  val sel_way = UInt(2.W) // if hit, the way index
  val branch_type = BpType()

  def target_pc_taken = bim_value(1) & valid

  def clear() = {
    valid := false.B
    target_pc := DontCare
    branch_offset := DontCare
    bim_value := DontCare
    is_rvc := DontCare
    sel_way := DontCare
    branch_type := DontCare
  }
}
class BPUTop(btb_way_count: Int, btb_nums: Int, bim_nums: Int) extends Module {

  require(
    isPow2(btb_way_count) && btb_way_count > 1,
    "btb_way_count should be power of 2"
  )
  require(isPow2(btb_nums) && btb_nums > 1, "btb_nums should be power of 2")
  require(isPow2(bim_nums) && bim_nums > 1, "bim_nums should be power of 2")

  val io = IO(new Bundle {
    // lookup port
    val lookup_en = Input(Bool())
    val lookup_pc = Input(UInt(39.W)) // sv39
    val bp_info = Output(new BPInfo())
    // commit update port
    val cmt_update_pc = Input(UInt(39.W))
    val cmt_update_btb_en =
      Input(Bool()) // btb only update when branch predict fail
    val cmt_update_bim_en = Input(Bool()) // bim update for all branch
    val cmt_update_btb_data = Input(new BTBEntry())
    val cmt_update_bim_data = Input(UInt(2.W))
    val cmt_update_btb_way_sel = Input(UInt(log2Ceil(btb_way_count).W))
    // ifu f3 update port
    val f3_update_btb_pc = Input(UInt(39.W))
    val f3_update_btb_en =
      Input(Bool()) // btb only update when branch predict fail
    val f3_update_btb_data = Input(new BTBEntry())
    val f3_update_btb_way_sel = Input(UInt(log2Ceil(btb_way_count).W))

    // clear
    val clear_en = Input(Bool())
  })

  val btb = Module(new BTBNway(btb_way_count, btb_nums))
  val bim = Module(new BIM(bim_nums))

  // btb
  btb.io.lookup_en := io.lookup_en
  btb.io.lookup_pc := io.lookup_pc
  btb.io.refill_en := Mux(
    io.cmt_update_btb_en,
    io.cmt_update_btb_en,
    io.f3_update_btb_en
  )
  btb.io.refill_pc := Mux(
    io.cmt_update_btb_en,
    io.cmt_update_pc,
    io.f3_update_btb_pc
  )
  btb.io.refill_data := Mux(
    io.cmt_update_btb_en,
    io.cmt_update_btb_data,
    io.f3_update_btb_data
  )
  btb.io.refill_way_sel := Mux(
    io.cmt_update_btb_en,
    io.cmt_update_btb_way_sel,
    io.f3_update_btb_way_sel
  )

  btb.io.clear_en := io.clear_en

  // bim
  bim.io.lookup_en := io.lookup_en
  bim.io.lookup_pc := io.lookup_pc
  bim.io.refill_en := io.cmt_update_bim_en
  bim.io.refill_pc := io.cmt_update_pc
  bim.io.refill_data := io.cmt_update_bim_data
  bim.io.clear_en := io.clear_en

  // bim hit info
  val bim_value = bim.io.bim_value
  val bim_valid = bim.io.bim_valid

  io.bp_info.target_pc := HoldRegister(io.lookup_en, btb.io.target_pc, 1)
  io.bp_info.branch_offset := HoldRegister(
    io.lookup_en,
    btb.io.branch_offset,
    1
  )
  io.bp_info.bim_value := HoldRegister(io.lookup_en, bim_value, 1)
  io.bp_info.branch_type := HoldRegister(io.lookup_en, btb.io.branch_type, 1)
  io.bp_info.is_rvc := HoldRegister(io.lookup_en, btb.io.branch_is_rvc, 1)
  io.bp_info.sel_way := HoldRegister(io.lookup_en, btb.io.hit_way, 1)
  io.bp_info.valid := HoldRegister(
    io.lookup_en,
    btb.io.target_pc_hit & bim_valid,
    1
  )

}

object BPUTop extends App {
  GenVerilogHelper(
    new BPUTop(btb_way_count = 2, btb_nums = 64, bim_nums = 64)
  )
}
