package leesum
import chisel3._
import chisel3.util.MuxLookup
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
