package leesum.Cache

import chisel3._
import chisel3.util.{Mux1H, PopCount, ValidIO, isPow2, log2Ceil}
import leesum.GenVerilogHelper

object DCacheReqType extends ChiselEnum {
  val read = Value(0.U)
  val write = Value(1.U)
  val load_lookup = Value(2.U)
  val store_lookup = Value(3.U)
  val refill = Value(4.U)

  def need_lookup(req_type: DCacheReqType.Type): Bool = {
    VecInit(Seq(load_lookup.asUInt, store_lookup.asUInt))
      .contains(req_type.asUInt)
  }

  def need_read_tag(req_type: DCacheReqType.Type): Bool = {
    VecInit(Seq(read.asUInt, load_lookup.asUInt, store_lookup.asUInt))
      .contains(req_type.asUInt)
  }
  def need_read_data(req_type: DCacheReqType.Type): Bool = {
    VecInit(Seq(read.asUInt, load_lookup.asUInt))
      .contains(req_type.asUInt)
  }

  def need_read_dirty(req_type: DCacheReqType.Type): Bool = {
    VecInit(Seq(read.asUInt))
      .contains(req_type.asUInt)
  }

  def need_write_tag(req_type: DCacheReqType.Type): Bool = {
    VecInit(Seq(write.asUInt, refill.asUInt))
      .contains(req_type.asUInt)
  }

  def need_write_data(req_type: DCacheReqType.Type): Bool = {
    VecInit(Seq(write.asUInt, refill.asUInt))
      .contains(req_type.asUInt)
  }
  def need_write_dirty(req_type: DCacheReqType.Type): Bool = {
    VecInit(Seq(write.asUInt, refill.asUInt))
      .contains(req_type.asUInt)
  }

  def req_tag_en(req_type: DCacheReqType.Type): Bool = {
    val tag_read = need_read_tag(req_type)
    val tag_write = need_write_tag(req_type)
    assert(
      PopCount(Seq(tag_read, tag_write)) <= 1.U,
      "tag_read and tag_write should be mutually exclusive"
    )
    tag_read || tag_write
  }
  def req_data_en(req_type: DCacheReqType.Type): Bool = {
    val data_read = need_read_data(req_type)
    val data_write = need_write_data(req_type)
    assert(
      PopCount(Seq(data_read, data_write)) <= 1.U,
      "data_read and data_write should be mutually exclusive"
    )
    data_read || data_write
  }
  def req_dirty_en(req_type: DCacheReqType.Type): Bool = {
    val dirty_read = need_read_dirty(req_type)
    val dirty_write = need_write_dirty(req_type)
    assert(
      PopCount(Seq(dirty_read, dirty_write)) <= 1.U,
      "dirty_read and dirty_write should be mutually exclusive"
    )
    dirty_read || dirty_write
  }
}

class DCacheLine extends Bundle {
  val valid = Bool()
  val dirty = Bool()
  val tag = UInt(24.W)
  val data = UInt(128.W)
}

class DCacheNWay(way_nums: Int) extends Module {
  require(way_nums > 1, "way_nums should be positive")
  require(isPow2(way_nums), "way_nums should be power of 2")
  val way_bits = log2Ceil(way_nums)
  val io = IO(new Bundle {
    val req_valid = Input(Bool())
    val req_type = Input(DCacheReqType())
    val req_addr = Input(UInt(39.W)) // sv39
    val req_way = Input(UInt(way_bits.W))
    val read_data = Output(new DCacheLine())
    val lookup_hit_data = Output(UInt(128.W))
    val lookup_hit_way = Output(ValidIO(UInt(way_bits.W)))
    val write_data = Input(UInt(128.W))
    val write_mask = Input(UInt(16.W))

    // clear
    val clear_en = Input(Bool())
  })

  val tag_ways = Seq.fill(way_nums)(Module(new DCacheTag(29)))
  val data_ways = Seq.fill(way_nums)(Module(new DCacheData))
  val dirty_ways = Seq.fill(way_nums)(Module(new DCacheDirty))
  val req_filed = new LookupField(io.req_addr)

  val tag_reads = tag_ways.zipWithIndex.map { case (tag_array, i) =>
    tag_array.io.addr := req_filed.index
    tag_array.io.en := DCacheReqType.req_tag_en(io.req_type) && io.req_valid
    tag_array.io.wen := DCacheReqType.need_write_tag(
      io.req_type
    ) && (i.U === io.req_way) // only write the selected way
    tag_array.io.wdata := req_filed.tag
    tag_array.io.flush := io.clear_en // clear valid bit
    (tag_array.io.tag_valid, tag_array.io.rdata)
  }

  val data_reads = data_ways.zipWithIndex.map { case (data_array, i) =>
    data_array.io.addr := req_filed.index
    data_array.io.en := DCacheReqType.req_data_en(io.req_type) && io.req_valid
    data_array.io.wen := DCacheReqType.need_write_data(
      io.req_type
    ) && (i.U === io.req_way) // only write the selected way
    data_array.io.wdata := io.write_data
    data_array.io.wmask := io.write_mask
    data_array.io.rdata
  }

  val dirty_reads = dirty_ways.zipWithIndex.map { case (dirty_array, i) =>
    dirty_array.io.addr := req_filed.index
    dirty_array.io.en := DCacheReqType.req_dirty_en(io.req_type) && io.req_valid
    dirty_array.io.wen := DCacheReqType.need_write_dirty(
      io.req_type
    ) && (i.U === io.req_way) // only write the selected way
    dirty_array.io.wdata := Mux(
      io.req_type === DCacheReqType.write,
      true.B, // write type
      true.B // refill type TODO: when load, should be false
    )
    dirty_array.io.rdata
  }

  // hit info from N ways
  val tag_hits = VecInit(
    tag_reads
      .map { case (valid, tag) =>
        // use paddr to select the hit way
        valid && (tag === RegNext(req_filed.tag)) && DCacheReqType.need_lookup(
          RegNext(io.req_type)
        ) && RegNext(io.req_valid)
      }
  )
  // which way is hit
  val tag_hit_way =
    Mux1H(tag_hits, VecInit((0 until way_nums).map(_.U(way_bits.W))))
  // choose hit data
  val cache_hit_data = Mux1H(tag_hits, data_reads)
  require(cache_hit_data.getWidth == 128, "cache_hit_data should be 128 bits")

  io.lookup_hit_way.valid := tag_hits.reduce(_ || _)
  io.lookup_hit_way.bits := tag_hit_way
  io.lookup_hit_data := cache_hit_data

  io.read_data.valid := VecInit(tag_reads.map(_._1))(RegNext(io.req_way))
  io.read_data.tag := VecInit(tag_reads.map(_._2))(RegNext(io.req_way))
  io.read_data.data := VecInit(data_reads)(RegNext(io.req_way))
  io.read_data.dirty := VecInit(dirty_reads)(RegNext(io.req_way))

  // --------------------
  // assert
  // --------------------
  assert(io.req_way < way_nums.U, "req_way should be less than way_nums")
  assert(PopCount(tag_hits) <= 1.U, "tag_hits should be mutually exclusive")
}

object gen_DCacheNWay_verilog extends App {
  val way_nums = 4
  GenVerilogHelper(new DCacheNWay(way_nums))
}
