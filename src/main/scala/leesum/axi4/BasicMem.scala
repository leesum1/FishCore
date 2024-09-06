package leesum.axi4

import chisel3._
import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.util.{Cat, RegEnable, log2Ceil}
import leesum.Cache.DcacheConst
import leesum.Utils.HoldRegister
import leesum.{CheckAligned, GenVerilogHelper, writeByteArrayToStringsToFile}

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

class MemoryIO64to32(addr_width: Int) extends Module {
  val io = IO(new Bundle {
    val before = new BasicMemoryIO(addr_width, 64)
    val after = Flipped(new BasicMemoryIO(addr_width, 32))
  })

  io.after.i_we := io.before.i_we
  io.after.i_waddr := io.before.i_waddr

  io.after.i_wstrb := Mux(
    io.before.i_waddr(2),
    io.before.i_wstrb(7, 4),
    io.before.i_wstrb(3, 0)
  )
  io.after.i_wdata := Mux(
    io.before.i_waddr(2),
    io.before.i_wdata(63, 32),
    io.before.i_wdata(31, 0)
  )

  io.after.i_rd := io.before.i_rd
  io.after.i_raddr := io.before.i_raddr
  io.before.o_rdata := Mux(
    io.after.i_raddr(2),
    Cat(io.after.o_rdata(31, 0), 0.U(32.W)),
    Cat(0.U(32.W), io.after.o_rdata(31, 0))
  )

  // -------------------------
  // formal
  // -------------------------
  when(io.before.i_we) {
    assert(CheckAligned(io.before.i_waddr, DcacheConst.SIZE4))
    when(io.before.i_waddr(2)) {
      assert(
        io.before.i_wstrb(3, 0) === 0.U,
        "when addr[2] is 1, wstrb[3:0] must be 0"
      )
    }.otherwise {
      assert(
        io.before.i_wstrb(7, 4) === 0.U,
        "when addr[2] is 0, wstrb[7:4] must be 0"
      )
    }
  }
  when(io.before.i_rd) {
    assert(CheckAligned(io.before.i_raddr, DcacheConst.SIZE4))
  }

}

object gen_memory_io64to32_verilog extends App {
  GenVerilogHelper(new MemoryIO64to32(32))
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

  val mem_seq = contents_per_bit.indices
    .map { idx =>
      val mem = SyncReadMem(
        MEM_SIZE / 8,
        UInt(8.W)
      )
      if (contents.nonEmpty) {
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

  // keep the last read data
  io.o_rdata := HoldRegister(io.i_rd, read_data.asUInt, 1)
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

  // keep the last read data
  io.o_rdata := HoldRegister(io.i_rd, read_data.asUInt, 1)
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
