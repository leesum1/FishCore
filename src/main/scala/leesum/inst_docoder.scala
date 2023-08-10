package leesum

import chisel3.{Flipped, Module}
import chisel3.util.{BitPat, Decoupled}

class InstDecoder extends Module {
  val in = IO(Flipped(Decoupled(new INSTEntry)))

  val cur_inst = in.bits.inst

}
