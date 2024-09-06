package leesum.Cache

import chisel3._
import chisel3.util.{DecoupledIO, MuxLookup}
import leesum._
import leesum.axi4.AXIDef

object DcacheConst {
  val SIZE1 = 0.U(2.W)
  val SIZE2 = 1.U(2.W)
  val SIZE4 = 2.U(2.W)
  val SIZE8 = 3.U(2.W)

  val BURST_FIXED = 0.U(2.W)
  val BURST_INCR = 1.U(2.W)
  val BURST_WRAP = 2.U(2.W)
  val BURST_RESERVED = 3.U(2.W)

}

object DcacheSize2AxiSize {
  def apply(dcache_size: UInt): UInt = {
    require(dcache_size.getWidth == 2)
    val axi_size = MuxLookup(dcache_size, AXIDef.SIZE_1)(
      Seq(
        DcacheConst.SIZE1 -> AXIDef.SIZE_1,
        DcacheConst.SIZE2 -> AXIDef.SIZE_2,
        DcacheConst.SIZE4 -> AXIDef.SIZE_4,
        DcacheConst.SIZE8 -> AXIDef.SIZE_8
      )
    )
    axi_size
  }
}

class DCacheReq extends Bundle {
  val paddr = UInt(64.W)
  val size = UInt(2.W) // total bits == 2 ** size
  val wdata = UInt(64.W) // use GenAxiWdata to generate
  val wstrb = UInt(8.W) // use GenAxiWstrb to generate
  val is_store = Bool()
  val is_mmio = Bool()
  val id = UInt(8.W) // TODO: not implemented now

  def is_load = !is_store
}
class DCacheResp extends Bundle {
  val rdata = UInt(64.W) // // use GenAxiRdata to get right data
  val id = UInt(8.W) // TODO: not implemented now
  val exception = new ExceptionEntry()

}

class LoadDcacheReq extends Bundle {
  val paddr = UInt(64.W)
  val size = UInt(2.W)
  val is_mmio = Bool()

}
class LoadDcacheResp extends Bundle {
  // the valid data is indicated by the paddr, like AXI4
  val data = UInt(64.W) // use GenAxiRdata to get right data
  val exception = new ExceptionEntry(has_valid = true)
}
class StoreDcacheReq extends Bundle {
  val paddr = UInt(64.W)
  // wdata and wstrb indicate the valid data to be written, like AXI4
  val wdata = UInt(64.W) // use GenAxiWdata to generate
  val wstrb = UInt(8.W) // use GenAxiWstrb to generate
  val size = UInt(2.W)
  val is_mmio = Bool()

}
class StoreDcacheResp extends Bundle {
  val exception = new ExceptionEntry()

}

object DCacheConnect {
  def load_req_to_dcache_req(
      load_read: DecoupledIO[LoadDcacheReq],
      dcache_req: DecoupledIO[DCacheReq]
  ): Unit = {
    dcache_req.valid <> load_read.valid
    load_read.ready <> dcache_req.ready
    dcache_req.bits.paddr := load_read.bits.paddr
    dcache_req.bits.size := load_read.bits.size
    dcache_req.bits.is_store := false.B
    dcache_req.bits.is_mmio := load_read.bits.is_mmio
    dcache_req.bits.wdata := 0.U
    dcache_req.bits.wstrb := 0.U
    dcache_req.bits.id := 0.U
  }

  def store_req_to_dcache_req(
      store_read: DecoupledIO[StoreDcacheReq],
      dcache_req: DecoupledIO[DCacheReq]
  ): Unit = {
    dcache_req.valid <> store_read.valid
    store_read.ready <> dcache_req.ready
    dcache_req.bits.paddr := store_read.bits.paddr
    dcache_req.bits.size := store_read.bits.size
    dcache_req.bits.wdata := store_read.bits.wdata
    dcache_req.bits.wstrb := store_read.bits.wstrb
    dcache_req.bits.is_store := true.B
    dcache_req.bits.is_mmio := store_read.bits.is_mmio
    dcache_req.bits.id := 0.U
  }

  def dcache_resp_to_load_resp(
      dcache_resp: DecoupledIO[DCacheResp],
      load_resp: DecoupledIO[LoadDcacheResp]
  ): Unit = {
    load_resp.valid <> dcache_resp.valid
    dcache_resp.ready <> load_resp.ready
    load_resp.bits.data := dcache_resp.bits.rdata
    load_resp.bits.exception := dcache_resp.bits.exception
  }

  def dcache_resp_to_store_resp(
      dcache_resp: DecoupledIO[DCacheResp],
      store_resp: DecoupledIO[StoreDcacheResp]
  ): Unit = {
    store_resp.valid <> dcache_resp.valid
    dcache_resp.ready <> store_resp.ready
    store_resp.bits.exception := dcache_resp.bits.exception
  }

  def dcache_req_to_load_store_req(
      dcache_req: DecoupledIO[DCacheReq],
      load_req: DecoupledIO[LoadDcacheReq],
      store_req: DecoupledIO[StoreDcacheReq]
  ): Unit = {
    load_req.valid := dcache_req.valid && !dcache_req.bits.is_store
    store_req.valid := dcache_req.valid && dcache_req.bits.is_store
    dcache_req.ready := load_req.valid || store_req.valid
    load_req.bits.paddr := dcache_req.bits.paddr
    load_req.bits.size := dcache_req.bits.size
    load_req.bits.is_mmio := dcache_req.bits.is_mmio
    store_req.bits.paddr := dcache_req.bits.paddr
    store_req.bits.size := dcache_req.bits.size
    store_req.bits.wdata := dcache_req.bits.wdata
    store_req.bits.wstrb := dcache_req.bits.wstrb
    store_req.bits.is_mmio := dcache_req.bits.is_mmio
  }

  def dcache_resp_to_load_store_resp(
      last_is_store: Bool,
      load_resp: DecoupledIO[LoadDcacheResp],
      store_resp: DecoupledIO[StoreDcacheResp],
      dcache_resp: DecoupledIO[DCacheResp]
  ): Unit = {
    dcache_resp.valid := load_resp.valid || store_resp.valid
    dcache_resp.bits.rdata := Mux(!last_is_store, load_resp.bits.data, 0.U)
    dcache_resp.bits.exception := Mux(
      !last_is_store,
      load_resp.bits.exception,
      store_resp.bits.exception
    )
    dcache_resp.bits.id := 3.U

    load_resp.ready := dcache_resp.valid && !last_is_store
    store_resp.ready := dcache_resp.valid && last_is_store
  }

}

class ByteEnableGenerator extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(3.W))
    val wdata = Input(UInt(64.W))
    val size = Input(UInt(2.W))
    val byteEnable = Output(UInt(8.W))
    val wdata_aligned = Output(UInt(64.W))
    val checkAligned = Output(Bool())
  })
  io.byteEnable := GenAxiWstrb(io.addr, io.size)
  io.checkAligned := CheckAligned(io.addr, io.size)
  io.wdata_aligned := GenAxiWdata(io.wdata, io.addr)
}

object gen_test_verilog extends App {
  GenVerilogHelper(new ByteEnableGenerator())
}
