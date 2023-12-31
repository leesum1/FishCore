package leesum

import chisel3._
import leesum.TestUtils.{gen_axi_wstrb, sign_ext}
import org.scalacheck.Gen

import scala.language.implicitConversions

object TestUtils {

  def sign_ext(value: Long, inputWidth: Int, en: Boolean = true): Long = {
    require(
      1.to(64).contains(inputWidth),
      "inputWidth  must be in range [0, 64]"
    )
    if (inputWidth == 64) {
      return value
    }

    val sign_bit = (value >> (inputWidth - 1)) & 0x1L
    val value_mask = (1L << inputWidth) - 1L
    val value_masked = value & value_mask
    if (sign_bit == 0 || !en) {
      value_masked
    } else {
      val sign_mask = (1L << (64 - inputWidth)) - 1L

      val sign_masked = value_masked & sign_mask
      val sign_ext = sign_masked | (sign_mask << inputWidth)
      sign_ext
    }

  }

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

  val sign_test1 = sign_ext(0xf, 8)
  val sign_test2 = sign_ext(0x7a, 7)

  println(sign_test1.toHexString)
  println(sign_test2.toHexString)

  println(wstrb.toBinaryString)
}

import chisel3.util._
import chiseltest._

class ExtendedDecoupledDriver[T <: Data](x: ReadyValidIO[T]) {
  def expectDequeueAdditional(data: T, clock: Clock)(
      addition_expect: => Unit
  ): Unit = timescope {
    // TODO: check for init
    x.ready.poke(true.B)
    fork
      .withRegion(Monitor) {
        x.waitForValid()
        x.valid.expect(true.B)
        x.bits.expect(data)
        addition_expect
      }
      .joinAndStep(clock)
  }

  def enqueueAdditional(data: T, clock: Clock)(
      addition_poke: => Unit
  ): Unit = timescope {
    x.bits.poke(data)
    x.valid.poke(true.B)
    addition_poke
    fork
      .withRegion(Monitor) {
        while (!x.ready.peek().litToBoolean) {
          clock.step(1)
        }
      }
      .joinAndStep(clock)
  }

}
object ExtendedDecoupledDriverImplicits {
  implicit def toExtendedDriver[T <: Data](
      x: ReadyValidIO[T]
  ): ExtendedDecoupledDriver[T] = {
    new ExtendedDecoupledDriver(x)
  }
}
