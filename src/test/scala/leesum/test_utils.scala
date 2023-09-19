package leesum

import chisel3._
import org.scalacheck.Gen

object test_utils {
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

  def gen_rand_uint(width: Int) = {
    // Generate a random hex string with 8 characters and prefix it with "x"
    // such as "x12345678", "xabcdef12", etc.
    // and than convert it to UInt(32.W)

    require(width % 8 == 0, "width must be a multiple of 8")
    require(width <= 64, "width must be less than 64")
    val uint_gen =
      Gen
        .listOfN(width / 8, Gen.hexChar)
        .map("x" + _.mkString)
        .map(_.U(width.W))

    uint_gen
  }
}
