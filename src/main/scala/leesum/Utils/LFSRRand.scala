package leesum.Utils

import chisel3._
import chisel3.util.random.LFSR
import chisel3.util.{isPow2, log2Ceil}

object LFSRRand {
  def apply(count: Int): UInt = {
    require(count > 0)
    require(isPow2(count), "max must be power of 2")
    val max_width = log2Ceil(count)

    val rand = if (max_width == 1) {
      LFSR(2)(0)
    } else {
      LFSR(max_width)
    }
    assert(rand < count.U, "rand must be less than count")
    rand
  }
}
