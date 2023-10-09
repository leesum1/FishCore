package leesum.axi4

import chisel3._
import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.util.{Cat, RegEnable, log2Ceil}
import leesum.{GenVerilogHelper, writeByteArrayToStringsToFile}

import java.nio.file.{Files, Paths}

class BasicMemoryIO(ADDR_WIDTH: Int, DATA_WIDTH: Int) extends Bundle {
  val i_we = Input(Bool())
  val i_wstrb = Input(UInt((DATA_WIDTH / 8).W))
  val i_waddr = Input(UInt(ADDR_WIDTH.W))
  val i_wdata = Input(UInt(DATA_WIDTH.W))
  val i_rd = Input(Bool())
  val i_raddr = Input(UInt(ADDR_WIDTH.W))
  val o_rdata = Output(UInt(DATA_WIDTH.W))
}

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
class BasicMemorySyncReadMem(
    DATA_WIDTH: Int,
    BASE_ADDR: Long,
    MEM_SIZE: Long = 2048,
    memoryFile: String = ""
) extends Module {

  val addr_width = log2Ceil(BASE_ADDR + MEM_SIZE)

  val io = IO(new BasicMemoryIO(addr_width, DATA_WIDTH))

  require(DATA_WIDTH % 8 == 0, "DATA_WIDTH must be a multiple of 8")

  def mem_addr(addr: UInt): UInt = {
    ((addr - BASE_ADDR.U) >> 3).asUInt
  }

  // -------------------------
  // init memory from file
  // -------------------------
  val contentsDelayed = if (memoryFile.trim.nonEmpty) {
    val byteArray = Files.readAllBytes(Paths.get(memoryFile))
    byteArray.toSeq
  } else {
    Seq.empty[Byte]
  }

  val contents = contentsDelayed
  val beatBytes = DATA_WIDTH / 8
  require(contents.size <= MEM_SIZE)

  val contents_per_bit =
    0.until(beatBytes)
      .map(idx => contents.drop(idx).sliding(1, beatBytes).flatten.toSeq)

  if (!contents.isEmpty) {
    contents_per_bit.zipWithIndex.foreach { case (content, idx) =>
      val file_name = s"src/main/resources/BasicMemorySyncReadMemInit_$idx.txt"
      writeByteArrayToStringsToFile(file_name, content.toArray)
    }
  }

  val mem_seq = 0
    .until(contents_per_bit.size)
    .map { case idx =>
      val mem = SyncReadMem(
        MEM_SIZE / 8,
        UInt(8.W)
      )
      if (!contents.isEmpty) {
        loadMemoryFromFileInline(
          mem,
          s"src/main/resources/BasicMemorySyncReadMemInit_$idx.txt"
        )
      }
      mem
    }

  val read_seq = mem_seq
    .map(mem => {
      val read_addr = mem_addr(io.i_raddr)
      val read_data = mem.read(read_addr)
      read_data
    })

  val read_data = Cat(read_seq.reverse)

  require(read_data.getWidth == DATA_WIDTH)

  when(io.i_we) {
    val waddr = mem_addr(io.i_waddr)
    io.i_wstrb.asBools.zipWithIndex.foreach { case (strb, idx) =>
      when(strb) {
        mem_seq(idx).write(waddr, io.i_wdata(idx * 8 + 7, idx * 8))
      }
    }
  }

  val prevRdataReg = RegInit(
    0.U(DATA_WIDTH.W)
  ) // Register to hold previous output value

  // Register to hold previous output value
  when(RegNext(io.i_rd)) {
    prevRdataReg := read_data.asUInt
  }

  io.o_rdata := Mux(RegNext(io.i_rd), read_data.asUInt, prevRdataReg)
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
class BasicMemoryVec(
    DATA_WIDTH: Int,
    BASE_ADDR: Long,
    MEM_SIZE: Long = 2048,
    memoryFile: String = ""
) extends Module {
  val addr_width = log2Ceil(BASE_ADDR + MEM_SIZE)

  val io = IO(new BasicMemoryIO(addr_width, DATA_WIDTH))

  require(DATA_WIDTH % 8 == 0, "DATA_WIDTH must be a multiple of 8")

  def mem_addr(addr: UInt): UInt = {
    ((addr - BASE_ADDR.U) >> 3).asUInt
  }
  // -------------------------
  // init memory from file
  // -------------------------
  val contentsDelayed = if (memoryFile.trim.nonEmpty) {
    val byteArray = Files.readAllBytes(Paths.get(memoryFile))
    byteArray.toSeq
  } else {
    Seq.empty[Byte]
  }
  // reference: https://github.com/chipsalliance/rocket-chip/blob/master/src/main/scala/devices/tilelink/BootROM.scala#L44
  val contents = contentsDelayed
  val beatBytes = DATA_WIDTH / 8
  require(contents.size <= MEM_SIZE)

  // pad unused space with zeros
  val words = (contents ++ Seq.fill(MEM_SIZE.toInt - contents.size)(0.toByte))
    .grouped(beatBytes)
    .toSeq

  val vec_mem = words.map(x => {
    val vec = VecInit(x.map { x =>
      (x & 0xff).U(8.W)
    })
    vec
  })

  val mem = RegInit(VecInit(vec_mem))
  require(mem.size == MEM_SIZE / beatBytes, "memory size mismatch")
  println("BasicMemoryVec Size: " + MEM_SIZE)

  // -------------------------
  // Read logic
  // -------------------------
  val read_addr = mem_addr(io.i_raddr)
  val read_data = RegEnable(
    mem(read_addr),
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
    0.U(DATA_WIDTH.W)
  ) // Register to hold previous output value

  // Register to hold previous output value
  when(RegNext(io.i_rd)) {
    prevRdataReg := read_data.asUInt
  }

  io.o_rdata := Mux(RegNext(io.i_rd), read_data.asUInt, prevRdataReg)
}

/** Because of Chisel's SyncReadMem does not support initial memory, so we use
  * self-defined memory module instead.
  * @param ADDR_WIDTH
  * @param DATA_WIDTH
  * @param BASE_ADDR
  * @param memoryFile
  */
class BasicMemory(
    DATA_WIDTH: Int,
    BASE_ADDR: Long,
    MEM_SIZE: Long,
    memoryFile: String = "",
    use_sync_mem: Boolean = true
) extends Module {
  val addr_width = log2Ceil(BASE_ADDR + MEM_SIZE)

  val io = IO(new BasicMemoryIO(addr_width, DATA_WIDTH))
  if (use_sync_mem) {
    val mem =
      Module(
        new BasicMemorySyncReadMem(
          DATA_WIDTH,
          BASE_ADDR,
          MEM_SIZE,
          memoryFile
        )
      )
    io <> mem.io
  } else {
    val memVec = Module(
      new BasicMemoryVec(DATA_WIDTH, BASE_ADDR, MEM_SIZE, memoryFile)
    )
    io <> memVec.io
  }
}

object gen_basic_mem_verilog extends App {
  GenVerilogHelper(
    new BasicMemory(
      DATA_WIDTH = 64,
      BASE_ADDR = 0x80000000L,
      MEM_SIZE = 2048,
      memoryFile = "src/main/resources/random_file.bin"
    )
  )
}
