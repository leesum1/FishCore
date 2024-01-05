package leesum.bpu

import chisel3._
import chisel3.util.{Mux1H, PopCount, isPow2, log2Ceil}
import leesum.{BpType, GenVerilogHelper}
import leesum.Utils.{LFSRRand, SinglePortSRAM}

class BTBEntry extends Bundle {
  val target_pc = UInt(39.W)
  val offset = UInt(3.W) // the first jump instruction offset
  val bp_type = BpType()
}

class BTBPCField(pc: UInt, nums: Int) {
  require(isPow2(nums) && nums > 1, "nums should be power of 2")
  val fetch_group_size = 8
  val offset = pc(log2Ceil(fetch_group_size) - 1, 0) // inst offset
  val index =
    pc(
      log2Ceil(nums) + log2Ceil(fetch_group_size) - 1,
      log2Ceil(fetch_group_size)
    )
  val tag = pc(pc.getWidth - 1, log2Ceil(nums) + log2Ceil(fetch_group_size))
}

class BTBNway(way_count: Int = 2, nums: Int) extends Module {
  val io = IO(new Bundle {
    // lookup
    val lookup_en = Input(Bool())
    val lookup_pc = Input(UInt(39.W)) // sv39
    val target_pc = Output(UInt(39.W))
    val target_pc_hit = Output(Bool())
    // refill
    val refill_en = Input(Bool())
    val refill_pc = Input(UInt(39.W))
    val refill_data = Input(new BTBEntry())
    val refill_way_sel =
      Input(UInt(log2Ceil(way_count).W)) // choose way to refill
    // clear
    val clear_en = Input(Bool())
  })
  require(isPow2(way_count) && way_count > 1, "way_count should be power of 2")
  require(isPow2(nums) && nums > 1, "nums should be power of 2")

  val lookup_pc_field = new BTBPCField(io.lookup_pc, nums)
  val refill_pc_field = new BTBPCField(io.refill_pc, nums)

  val btb_valids =
    Seq.fill(way_count)(RegInit(VecInit(Seq.fill(nums)(false.B))))
  val btb_tags =
    Seq.fill(way_count)(
      Module(new SinglePortSRAM(nums, UInt(lookup_pc_field.tag.getWidth.W)))
    )
  val btb_datas =
    Seq.fill(way_count)(Module(new SinglePortSRAM(nums, new BTBEntry())))

  // read tag from N ways
  val tag_rdatas = btb_tags.zipWithIndex.map { case (tag_array, i) =>
    tag_array.io.addr := Mux(
      io.refill_en,
      refill_pc_field.index,
      lookup_pc_field.index
    )
    tag_array.io.en := io.lookup_en || io.refill_en
    tag_array.io.wen := io.refill_en && (i.U === io.refill_way_sel)
    tag_array.io.wdata := refill_pc_field.tag

    val tag_valid = btb_valids(i)(lookup_pc_field.index)

    when(io.refill_en && (i.U === io.refill_way_sel)) {
      btb_valids(i)(refill_pc_field.index) := true.B
    }

    when(io.clear_en) {
      btb_valids(i).foreach(_ := false.B)
    }

    (tag_valid, tag_array.io.rdata)
  }

  // read data from N ways
  val data_rdatas = btb_datas.zipWithIndex.map { case (data_array, i) =>
    data_array.io.addr := Mux(
      io.refill_en,
      refill_pc_field.index,
      lookup_pc_field.index
    )
    data_array.io.en := io.lookup_en || io.refill_en
    data_array.io.wen := io.refill_en && (i.U === io.refill_way_sel)
    data_array.io.wdata := io.refill_data
    data_array.io.rdata
  }

  // hit info from N ways
//  val tag_and_offset_hits_ = VecInit(
//    tag_rdatas
//      .map { case (valid, tag) =>
//        // one cycle delay
//        valid && io.lookup_en && (tag === RegNext(lookup_pc_field.tag))
//      }
//  )

  val tag_and_offset_hits = VecInit(
    tag_rdatas
      .zip(data_rdatas)
      .map { case ((valid, tag), data) =>
        // use lookup pc one cycle delay
        val tag_hit = tag === RegNext(lookup_pc_field.tag)
        val offset_hit = data.offset >= RegNext(lookup_pc_field.offset)
        valid && io.lookup_en && tag_hit && offset_hit
      }
  )

  // choose  hit data
  val btb_rdata = Mux1H(tag_and_offset_hits, data_rdatas)

  io.target_pc := btb_rdata.target_pc
  io.target_pc_hit := tag_and_offset_hits.reduce(_ || _)

  // ------------------------
  // assert
  // ------------------------
  assert(
    PopCount(tag_and_offset_hits) <= 1.U,
    "tag hit can't be more than 1"
  )
  assert(
    PopCount(
      Seq(io.refill_en, io.clear_en, io.lookup_en)
    ) <= 1.U,
    "refill and clear can't be enabled at the same time"
  )
}

object gen_BTBNway_verilog extends App {
  GenVerilogHelper(new BTBNway(way_count = 4, nums = 32))
}
