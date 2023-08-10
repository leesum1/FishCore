package leesum.axi4
import chisel3._
import chisel3.experimental.IO
import chisel3.stage.ChiselStage
import chisel3.util.{
  Decoupled,
  Enum,
  HasBlackBoxResource,
  MuxLookup,
  PopCount,
  Reverse,
  is
}
import leesum.axi4.AXIDef.{
  BURST_FIXED,
  SIZE_1,
  SIZE_128,
  SIZE_16,
  SIZE_2,
  SIZE_32,
  SIZE_4,
  SIZE_64,
  SIZE_8
}

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
// TODO: implement wrap burst
// TODO: alignmentMask should depend on data width,which could reduce area
class AXIAddr(ADDR_WIDTH: Int, DATA_WIDTH: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(ADDR_WIDTH.W))
    val len = Input(UInt(8.W))
    val size = Input(UInt(3.W))
    val burst = Input(UInt(2.W))
    val next_addr = Output(UInt(ADDR_WIDTH.W))
  })

  val addr_width = ADDR_WIDTH

  val inc =
    Mux(io.burst === BURST_FIXED, 0.U, AXIDef.get_increment_size(io.size))

  assert(inc <= DATA_WIDTH.U, "increment size should be less than data width")

  val next_addr_tmp = io.addr + inc
  val alignmentMask = MuxLookup(
    io.size,
    0.U,
    Array(
      SIZE_1 -> ~"b000".U(addr_width.W),
      SIZE_2 -> ~"b001".U(addr_width.W),
      SIZE_4 -> ~"b011".U(addr_width.W),
      SIZE_8 -> ~"b111".U(addr_width.W),
      SIZE_16 -> ~"b1111".U(addr_width.W),
      SIZE_32 -> ~"b11111".U(addr_width.W),
      SIZE_64 -> ~"b111111".U(addr_width.W),
      SIZE_128 -> ~"b1111111".U(addr_width.W)
    )
  )

  require(alignmentMask.getWidth == addr_width)

  val aligned_next_addr = next_addr_tmp & alignmentMask.asUInt
  io.next_addr := aligned_next_addr
}

object gen_verilog extends App {
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new AXIAddr(12, 64),
      Array(
        "--target-dir",
        verilogDir
      )
    )

}
