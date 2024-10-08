package leesum.axi4
import chisel3._
import chisel3.util.{HasBlackBoxResource, MuxLookup}
import leesum.axi4.AXIDef._
import leesum.{GenMaskZero, GenVerilogHelper}

class axi_addr(ADDR_WIDTH: Int, DATA_WIDTH: Int)
    extends BlackBox(Map("AW" -> ADDR_WIDTH, "DW" -> DATA_WIDTH))
    with HasBlackBoxResource {
  val io = IO(new Bundle {
    val i_last_addr = Input(UInt(ADDR_WIDTH.W))
    val i_len = Input(UInt(8.W))
    val i_size = Input(UInt(3.W))
    val i_burst = Input(UInt(2.W))
    val o_next_addr = Output(UInt(ADDR_WIDTH.W))
  })
  addResource("/axi_addr.v")
}

class back_box_test(ADDR_WIDTH: Int, DATA_WIDTH: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(ADDR_WIDTH.W))
    val len = Input(UInt(8.W))
    val size = Input(UInt(3.W))
    val burst = Input(UInt(2.W))
    val next_addr = Output(UInt(ADDR_WIDTH.W))
  })

  val ref = Module(new axi_addr(ADDR_WIDTH, DATA_WIDTH))
  ref.io.i_last_addr := io.addr
  ref.io.i_len := io.len
  ref.io.i_size := io.size
  ref.io.i_burst := io.burst
  io.next_addr := ref.io.o_next_addr
}

// TODO: implement wrap burst
// TODO: alignmentMask should depend on data width,which could reduce area
class AXIAddr(ADDR_WIDTH: Int, DATA_WIDTH: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(ADDR_WIDTH.W))
    val len = Input(UInt(8.W))
    val size = Input(UInt(3.W))
    val burst = Input(UInt(2.W))
    val next_addr = Output(UInt(ADDR_WIDTH.W))

    def clear(): Unit = {
      addr := 0.U
      len := 0.U
      size := 0.U
      burst := 0.U
    }
  })

  val addr_width = ADDR_WIDTH

  val inc =
    Mux(io.burst === BURST_FIXED, 0.U, AXIDef.get_increment_size(io.size))

  assert(inc <= DATA_WIDTH.U, "increment size should be less than data width")

  val next_addr_tmp = io.addr + inc

  val alignmentMask = MuxLookup(
    io.size,
    0.U
  )(
    Seq(
      SIZE_1 -> GenMaskZero(addr_width, 0),
      SIZE_2 -> GenMaskZero(addr_width, 1),
      SIZE_4 -> GenMaskZero(addr_width, 2),
      SIZE_8 -> GenMaskZero(addr_width, 3),
      SIZE_16 -> GenMaskZero(addr_width, 4),
      SIZE_32 -> GenMaskZero(addr_width, 5),
      SIZE_64 -> GenMaskZero(addr_width, 6),
      SIZE_128 -> GenMaskZero(addr_width, 7)
    )
  )
  require(alignmentMask.getWidth == addr_width)

  val aligned_next_addr = next_addr_tmp & alignmentMask.asUInt
  io.next_addr := aligned_next_addr
}

object gen_AXIAddr_verilog extends App {
  GenVerilogHelper(new AXIAddr(ADDR_WIDTH = 12, DATA_WIDTH = 64))
}
