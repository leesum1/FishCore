package leesum

import chisel3._

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
}
