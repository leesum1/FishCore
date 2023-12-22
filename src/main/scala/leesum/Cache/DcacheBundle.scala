package leesum
import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, MuxLookup}
import leesum.axi4.AXIDef

import scala.math.Equiv

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
  val size = UInt(2.W)
  val wdata = UInt(64.W)
  val wstrb = UInt(8.W)
  val is_store = Bool()
  val is_mmio = Bool()
  val id = UInt(8.W) // TODO: not implemented now

  def is_load = !is_store

}
class DCacheResp extends Bundle {
  val rdata = UInt(64.W)
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
  val data = UInt(64.W)
  val exception = new ExceptionEntry(has_valid = true)
}
class StoreDcacheReq extends Bundle {
  val paddr = UInt(64.W)
  // wdata and wstrb indicate the valid data to be written, like AXI4
  val wdata = UInt(64.W)
  val wstrb = UInt(8.W)
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
