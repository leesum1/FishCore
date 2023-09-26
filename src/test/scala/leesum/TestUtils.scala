package leesum

import chisel3._
import leesum.TestUtils.gen_axi_wstrb
import org.scalacheck.Gen

object TestUtils {
  def long2UInt64(x: Long): UInt = {
    if (x < 0) {
      // because of chisel doesn't support convert a negative number to UInt
      // so we first convert Long to hex string(with prefix x)
      // then convert it to UInt
      val hex_string: String = "x" + x.toHexString
      hex_string.U(64.W)
    } else {
      x.U(64.W)
    }
  }

  def int2UInt32(x: Int): UInt = {
    if (x < 0) {
      // because of chisel doesn't support convert a negative number to UInt
      // so we first convert Long to hex string(with prefix x)
      // then convert it to UInt
      val hex_string: String = "x" + x.toHexString
      hex_string.U(32.W)
    } else {
      x.U(32.W)
    }
  }

  def int2UInt64(x: Int): UInt = {
    long2UInt64(int2unsignedInt(x))
  }

  def int2unsignedInt(x: Int): Long = {
    x & 0xffffffffL
  }

  /** Convert a signed long to unsigned long (use BigDecimal)
    *
    * @param value
    * @return
    */
  def long2Ulong(value: Long): BigDecimal = {
    if (value >= 0) {
      return BigDecimal(value)
    }
    val lowValue = value & Long.MaxValue

    BigDecimal
      .valueOf(lowValue) + BigDecimal.valueOf(Long.MaxValue) + BigDecimal
      .valueOf(1)
  }

  def gen_rand_uint(width: Int): Gen[UInt] = {
    require(width > 0, "width must be greater than 0")

    val uint_gen =
      Gen
        .listOfN(width, Gen.oneOf('0', '1'))
        .map("b" + _.mkString)
        .map(_.U(width.W))

    uint_gen
  }

  // size: 0 -> 1 byte, 1 -> 2 bytes, 2 -> 4 bytes, 3 -> 8 bytes
  def check_aligned(addr: Long, size: Int): Boolean = {
    require(
      size >= 0 && size <= 3,
      "size must be in range [0, 3]"
    )
    (addr & ((1 << size) - 1)) == 0
  }

  def byteSeq2Uint64LittleEndian(bytes: Seq[Byte]): Long = {
    require(bytes.nonEmpty)
    bytes.zipWithIndex.foldLeft(0L) { case (acc, (b, i)) =>
      acc | ((b & 0xffL) << (i * 8))
    }
  }

  def gen_axi_wdata(data: Long, addr: Long): Long = {
    val offset = addr & 0x7
    data << (offset * 8)
  }
  def gen_axi_wstrb(addr: Long, size: Int): Int = {
    val offset = addr & 0x7
    require(size >= 0 && size <= 3, "size must be in range [0, 3]")
    val real_size = 1 << size
    val mask = (1 << real_size) - 1
    (mask << offset) & 0xff
  }
}

object rand_test1 extends App {
  val uint_gen = TestUtils.gen_rand_uint(4)
  val wstrb = gen_axi_wstrb(0x1002, 2)
  println(wstrb.toBinaryString)
}
