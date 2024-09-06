package leesum.Cache

import chisel3._
import chisel3.util.{Mux1H, PopCount, Valid, ValidIO, isPow2}
import leesum.GenVerilogHelper
import leesum.Utils.LFSRRand

class LookupField(x: UInt) {
  require(x.getWidth == 39) // sv39
  val tag = x(38, 10) // 29
  val index = x(9, 4) // 6
  val offset = x(3, 0) // 4

  // aligned by 8 bytes
  val aligned_offset = x(3) // TODO: bugs here!!! width should by way_count
}

class ICache2way(way_count: Int = 2) extends Module {
  val io = IO(new Bundle {
    // lookup
    val lookup_en = Input(Bool())
    val addr = Input(UInt(39.W)) // sv39
    val plookup_tag = Input(UInt(29.W)) // physical address tag
    val cache_data = Output(UInt(64.W))
    val cache_data_hit = Output(Bool())
    // refill
    val refill_en = Input(Bool())
    val refill_index = Input(UInt(6.W))
    val refill_data = Input(UInt(128.W))
    val refill_tag = Input(UInt(24.W))
    // clear
    val clear_en = Input(Bool())
  })
  require(isPow2(way_count), "way_count should be power of 2")
  require(way_count == 2, "only support 2 way cache")

  val vlookup_field = new LookupField(io.addr)

  val aligned_offset_buf = RegInit(0.U(vlookup_field.aligned_offset.getWidth.W))
  when(io.lookup_en) {
    aligned_offset_buf := vlookup_field.aligned_offset
  }

  val tag_ways = Seq.fill(way_count)(Module(new ICacheTag(29)))
  val data_ways = Seq.fill(way_count)(Module(new ICacheData))
  // use lfsr to randomize the way when refill
  val rand_value = LFSRRand(way_count)

  // read tag from 2 ways
  val tag_rdatas = tag_ways.zipWithIndex.map { case (tag_array, i) =>
    tag_array.io.addr := Mux(io.refill_en, io.refill_index, vlookup_field.index)
    tag_array.io.en := io.lookup_en || io.refill_en
    tag_array.io.wen := io.refill_en && (i.U === rand_value)
    tag_array.io.wdata := io.refill_tag
    tag_array.io.flush := io.clear_en
    (tag_array.io.tag_valid, tag_array.io.rdata)
  }
  // read data from 2 ways
  val data_rdatas = data_ways.zipWithIndex.map { case (data_array, i) =>
    data_array.io.addr := Mux(
      io.refill_en,
      io.refill_index,
      vlookup_field.index
    )
    data_array.io.en := io.lookup_en || io.refill_en
    data_array.io.wen := io.refill_en && (i.U === rand_value)
    data_array.io.wdata := io.refill_data
    data_array.io.rdata
  }

  // hit info from 2 ways
  val tag_hits = VecInit(
    tag_rdatas
      .map { case (valid, tag) =>
        // use paddr to select the hit way
        valid && (tag === io.plookup_tag)
      }
  )
  // choose  hit data
  val cache_rdata =
    Mux1H(tag_hits, data_rdatas).asTypeOf(Vec(way_count, UInt(64.W)))(
      aligned_offset_buf
    )

  io.cache_data := cache_rdata
  io.cache_data_hit := tag_hits.reduce(_ || _)

  // ------------------------
  // assert
  // ------------------------
  assert(
    PopCount(
      tag_rdatas
        .map { case (valid, tag) =>
          valid && (tag === io.plookup_tag)
        }
    ) <= 1.U,
    "tag hit can't be more than 1"
  )
  assert(
    PopCount(
      Seq(io.refill_en, io.clear_en, io.lookup_en)
    ) <= 1.U,
    "refill and clear can't be enabled at the same time"
  )
}
object gen_ICache2way_verilog extends App {
  GenVerilogHelper(new ICache2way())
}
