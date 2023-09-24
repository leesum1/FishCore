package leesum
import chisel3._
import chiseltest._
import leesum.axi4.BasicMemory
import leesum.test_utils.long2UInt64
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
// TODO: when read and write the same address, there are two tactics
// 1. write first, then read
// 2. read first, then write
class RefMemory(ADDR_WIDTH: Int, DATA_WIDTH: Int, BASE_ADDR: Int) {
  require(DATA_WIDTH % 8 == 0, "DATA_WIDTH must be a multiple of 8")
  var mem = Array.ofDim[Byte](1 << ADDR_WIDTH, DATA_WIDTH / 8)
  for (i <- 0 until (1 << ADDR_WIDTH)) {
    for (j <- 0 until (DATA_WIDTH / 8)) {
      mem(i)(j) = 0
    }
  }

  def mem_addr(addr: Int): Int = {
    ((addr - BASE_ADDR) >> 3)
  }
  def write(addr: Int, data: Long, wstrb: Int): Unit = {
    val addrIndex = mem_addr(addr)

    val dataBytes =
      (0 until (DATA_WIDTH / 8)).map(i => ((data >> (i * 8)) & 0xff).toByte)
    // little endian
    dataBytes.zipWithIndex
      .foreach { case (dataByte, index) =>
        // check wstrb mask
        if ((wstrb & (1 << index)) != 0) {
          mem(addrIndex)(index) = dataByte
        }
      }
  }

  def read(addr: Int): Long = {
    val addrIndex = mem_addr(addr)
    // little endian
    val data = mem(addrIndex).zipWithIndex
      .map { case (byte, index) =>
        (byte.toLong & 0xff) << (index * 8)
      }
      .reduce(_ | _)

    data
  }

}

// TODO: when read and write the same address, there are two tactics
// 1. write first, then read
// 2. read first, then write
// These two tactics were not tested in this test
class BasicMemoryTest extends AnyFreeSpec with ChiselScalatestTester {
  def basic_mem_write_gen(ADDR_WIDTH: Int, DATA_WIDTH: Int, BASE_ADDR: Int) = {
    val addr_gen = Gen.choose(BASE_ADDR, (1 << ADDR_WIDTH) - 1 - BASE_ADDR)
    val data_gen = Gen.choose(Long.MinValue, Long.MaxValue)
    val strb_gen = Gen.choose(0, (1 << (DATA_WIDTH / 8)) - 1)

    for {
      addr <- addr_gen
      data <- data_gen
      strb <- strb_gen
    } yield {
      (
        (addr + BASE_ADDR).U(ADDR_WIDTH.W),
        long2UInt64(data),
        strb.U((DATA_WIDTH / 8).W)
      )
    }
  }
  def basic_mem_read_gen(ADDR_WIDTH: Int, BASE_ADDR: Int) = {
    val addr_gen = Gen.choose(BASE_ADDR, (1 << ADDR_WIDTH) - 1 - BASE_ADDR)
    for {
      addr <- addr_gen
    } yield {
      addr.U(ADDR_WIDTH.W)
    }
  }

  "read_after_write" in {
    test(
      new BasicMemory(
        ADDR_WIDTH = 12,
        DATA_WIDTH = 64,
        BASE_ADDR = 0,
        memoryFile =
          "/home/leesum/workhome/chisel-fish/src/main/resources/random_data_readmemh.txt"
      )
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut: BasicMemory =>
        val write_gen = basic_mem_write_gen(12, 64, 0)
        val read_gen = basic_mem_read_gen(12, 0)
        val write_seq = Gen.listOfN(10000, write_gen).sample.get
        val read_seq = Gen.listOfN(20000, read_gen).sample.get

        var ref_mem =
          new RefMemory(ADDR_WIDTH = 12, DATA_WIDTH = 64, BASE_ADDR = 0)

        dut.clock.step(10)

        write_seq.foreach(write => {
          // ref module
          ref_mem.write(
            write._1.litValue.toInt,
            write._2.litValue.toLong,
            write._3.litValue.toInt
          )
          dut_write(dut, write._1, write._2, write._3)
        })
        dut.clock.step(10)

        read_seq.foreach(read => {
          // ref module
          val ref_data = ref_mem.read(read.litValue.toInt)
          dut_read(dut, read, ref_data)
        })

      }
  }

  private def dut_read(dut: BasicMemory, addr: UInt, ref: Long): Unit = {
    timescope {
      dut.io.i_rd.poke(true.B)
      dut.io.i_raddr.poke(addr)
      dut.clock.step(1)
      dut.io.o_rdata.expect(long2UInt64(ref))
    }
  }
  private def dut_write(
      dut: BasicMemory,
      addr: UInt,
      data: UInt,
      strb: UInt
  ): Unit = {
    timescope {
      dut.io.i_we.poke(true.B)
      dut.io.i_waddr.poke(addr)
      dut.io.i_wdata.poke(data)
      dut.io.i_wstrb.poke(strb)
      dut.clock.step(1)
    }
  }
}
