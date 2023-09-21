package leesum.axi4

import chisel3._
import chisel3.util.{RegEnable, log2Ceil}
import chisel3.experimental
import chisel3.util.experimental.{loadMemoryFromFile, loadMemoryFromFileInline}
import circt.stage.ChiselStage
import leesum.GenVerilogHelper

import scala.io.Source

/** sync memory with one read port and one write port, with 1 latency of read or
  * write. use SyncReadMem as internal memory
  * @param ADDR_WIDTH
  *   address width in bits
  * @param DATA_WIDTH
  *   data width in bits
  * @param BASE_ADDR
  *   base address of memory
  * @param memoryFile
  *   binary file to load into memory(not support)
  */
class BasicMemory2(
    ADDR_WIDTH: Int,
    DATA_WIDTH: Int,
    BASE_ADDR: Long,
    memoryFile: String = ""
) extends Module {
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

  when(io.i_we) {
    mem.write(
      mem_addr(io.i_waddr),
      io.i_wdata.asTypeOf(Vec(DATA_WIDTH / 8, UInt(8.W))),
      io.i_wstrb.asTypeOf(Vec(DATA_WIDTH / 8, Bool()))
    )
  }

  // Register to hold previous output value
  when(RegNext(io.i_rd)) {
    prevRdataReg := read_data
  }

  io.o_rdata := Mux(RegNext(io.i_rd), read_data, prevRdataReg)
}

/** sync memory with one read port and one write port, with 1 latency of read or
  * write. use reg as internal memory, when building the circuit, must add
  * -Xss8m to increase stack size.
  * @param ADDR_WIDTH
  *   address width in bits
  * @param DATA_WIDTH
  *   data width in bits
  * @param BASE_ADDR
  *   base address of memory
  * @param memoryFile
  *   binary file to load into memory
  */
class BasicMemory(
    ADDR_WIDTH: Int,
    DATA_WIDTH: Int,
    BASE_ADDR: Long,
    memoryFile: String = ""
) extends Module {
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
  // -------------------------
  // init memory from file
  // -------------------------
  val contentsDelayed = if (!memoryFile.trim.isEmpty) {
    Source
      .fromFile(memoryFile)
      .map(_.toByte)
      .toSeq
  } else {
    Seq.empty[Byte]
  }
  // reference: https://github.com/chipsalliance/rocket-chip/blob/master/src/main/scala/devices/tilelink/BootROM.scala#L44
  val contents = contentsDelayed
  val addrSize = (1 << ADDR_WIDTH)
  val beatBytes = DATA_WIDTH / 8
  require(contents.size <= addrSize)

  // pad unused space with zeros
  val words = (contents ++ Seq.fill(addrSize - contents.size)(0.toByte))
    .grouped(beatBytes)
    .toSeq

  val vec1 = words.map(x => {
    val vec = VecInit(x.map(_.U(8.W)))
    vec
  })

  val mem = RegInit(VecInit(vec1))
  require(mem.size == addrSize / beatBytes, "memory size mismatch")
  println("vec1 size: " + vec1.size)
  println("addrSize: " + addrSize)

  // -------------------------
  // Read logic
  // -------------------------
  val read_data = RegEnable(
    mem(mem_addr(io.i_raddr)),
    io.i_rd
  )
  // -------------------------
  // Write logic
  // -------------------------
  when(io.i_we) {
    val write_data = io.i_wdata.asTypeOf(Vec(DATA_WIDTH / 8, UInt(8.W)))
    val write_mask = io.i_wstrb.asTypeOf(Vec(DATA_WIDTH / 8, Bool()))
    val write_addr = mem_addr(io.i_waddr)

    (write_data zip write_mask).zipWithIndex.foreach {
      case ((data, mask), index) =>
        when(mask) {
          mem(write_addr)(index) := data
        }
    }
  }

  val prevRdataReg = RegInit(
    0.U(32.W)
  ) // Register to hold previous output value

  // Register to hold previous output value
  when(RegNext(io.i_rd)) {
    prevRdataReg := read_data.asUInt
  }

  io.o_rdata := Mux(RegNext(io.i_rd), read_data.asUInt, prevRdataReg)
}

object gen_basic_mem_verilog extends App {
  GenVerilogHelper(
    new BasicMemory(
      ADDR_WIDTH = 12,
      DATA_WIDTH = 64,
      BASE_ADDR = 0,
      memoryFile = "src/main/resources/random_data_readmemh.txt"
    )
  )
}
