package leesum.axi4

import chisel3._
import chisel3.stage.ChiselStage

class BasicMemory(ADDR_WIDTH: Int, DATA_WIDTH: Int, BASE_ADDR: Int)
    extends Module {
  val io = IO(new Bundle {
    val i_we = Input(Bool())
    val i_wstrb = Input(UInt((DATA_WIDTH / 8).W))
    val i_waddr = Input(UInt(ADDR_WIDTH.W))
    val i_wdata = Input(UInt(DATA_WIDTH.W))
    val i_rd = Input(Bool())
    val i_raddr = Input(UInt(ADDR_WIDTH.W))
    val o_rdata = Output(UInt(DATA_WIDTH.W))
  })

  require(DATA_WIDTH % 8 == 0, "DATA_WIDTH must be a multiple of 8")

  def mem_addr(addr: UInt): UInt = {
    ((addr - BASE_ADDR.U) >> 3).asUInt
  }

  val mem = SyncReadMem(
    ((1 << ADDR_WIDTH) >> 3),
    Vec(DATA_WIDTH / 8, UInt(8.W))
  )

  val prevRdataReg = RegInit(
    0.U(32.W)
  ) // Register to hold previous output value

  val read_data = mem.read(mem_addr(io.i_raddr), io.i_rd).asUInt

  // Register to hold previous output value
  when(io.i_rd) {
    prevRdataReg := read_data
  }

  when(io.i_we) {
    mem.write(
      mem_addr(io.i_waddr),
      io.i_wdata.asTypeOf(Vec(DATA_WIDTH / 8, UInt(8.W))),
      io.i_wstrb.asTypeOf(Vec(DATA_WIDTH / 8, Bool()))
    )
  }

  io.o_rdata := Mux(io.i_rd, read_data, prevRdataReg)
}

object gen_axi_addr_verilog extends App {
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new BasicMemory(ADDR_WIDTH = 12, DATA_WIDTH = 64, BASE_ADDR = 0),
      Array(
        "--target-dir",
        verilogDir,
        "--emission-options=disableMemRandomization,disableRegisterRandomization"
      )
    )
}
