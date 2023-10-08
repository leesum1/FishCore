package leesum

import chisel3._
import chisel3.util.{Cat, Decoupled}

// TODO: make RspPacket configurable
class RspPacket extends Bundle {
  // config
  private val pc_width = 64
  private val payload_width = 64

  // inst_packet are aligned to 16 bits in memory
  private val inst_boundary = 16

  val max_inst_packet = payload_width / inst_boundary

  // port
  val pc = UInt(pc_width.W)
  val payload = UInt(64.W)

  /** A inst packet is 16 bits
    *
    * @param idx
    * @return
    */
  def inst_packet(idx: Int): UInt = {
    require(idx >= 0 && idx < max_inst_packet)
    val x = payload(16 * idx + 15, 16 * idx)
    require(x.getWidth == 16)
    x
  }
  def last_inst_packet: UInt = {
    inst_packet(max_inst_packet - 1)
  }
}

class InstsItem extends Bundle {
  val insts_vec = Output(Vec(4, new INSTEntry))

  def insts(idx: UInt): UInt = {
    insts_vec(idx).inst
  }
  def insts_pc(idx: UInt): UInt = {
    insts_vec(idx).pc
  }
  def insts_valid_mask(idx: UInt): Bool = {
    insts_vec(idx).valid
  }
  def insts_rvc_mask(idx: UInt): Bool = {

    insts_vec(idx).rvc
  }
  def clear(): Unit = {
    for (i <- 0 until 4) {
      val tmp = Wire(new INSTEntry)
      tmp.clear()
      insts_vec(i) := tmp
    }
  }
}

/** This module convert a RspPacket(8 byte size) to InstsItem. The InstsItem may
  * contain 1 to 4 insts, and the insts may be 16 bits or 32 bits. This module
  * is combination logic,Return the InstsItem at the same cycle when input fire.
  */
class InstReAlign extends Module {
  val input = IO(Flipped(Decoupled(new RspPacket)))

  val output = IO(
    new InstsItem
  )

  val last_half_inst = RegInit(0.U(16.W));
  val last_half_valid = RegInit(false.B)

  def is_occupied(idx: Int): Bool = {
    val ret = idx match {
      case -1 => !last_half_valid;
      case 0 =>
        val pre_inst_is_32bits =
          (!is_occupied(idx - 1)) & (!RiscvTools.is_rvc(
            Cat(
              input.bits.inst_packet(idx)(15, 0),
              last_half_inst
            )
          ));
        val current_is_rvc = (is_occupied(idx - 1)) & RiscvTools.is_rvc(
          input.bits.inst_packet(idx)
        );

        pre_inst_is_32bits || current_is_rvc;
      case _ =>
        val pre_inst_is_32bits =
          (!is_occupied(idx - 1)) & (!RiscvTools.is_rvc(
            input.bits.inst_packet(idx - 1)
          ))
        val current_is_rvc = is_occupied(idx - 1) & RiscvTools.is_rvc(
          input.bits.inst_packet(idx)
        )

        pre_inst_is_32bits || current_is_rvc
    }

    if (idx < 0) {
      ret.suggestName(s"occupied_neg_${idx.abs}")
    } else {
      ret.suggestName(s"occupied_${idx}")
    }
    ret
  }

  // buffer last half inst
  when(input.fire) {
    last_half_inst := input.bits.inst_packet(3)
    when(is_occupied(3)) {
      last_half_valid := false.B
    }.otherwise {
      last_half_valid := true.B
    }
  }

  def realign_inst(
      cur_pc: UInt,
      pre_packet: UInt,
      cur_packet: UInt,
      pre_is_occupied: Bool,
      cur_is_occupied: Bool
  ): INSTEntry = {
    val aligned_inst = Wire(new INSTEntry)
    aligned_inst.clear()

    when(cur_is_occupied) {
      when(pre_is_occupied) {
        // 16 bits inst
        aligned_inst.valid := true.B
        aligned_inst.inst := RiscvTools.expand_rvc(cur_packet)
        aligned_inst.rvc := true.B
        aligned_inst.pc := cur_pc
      }.otherwise {
        // 32 bits inst
        aligned_inst.valid := true.B
        aligned_inst.inst := Cat(cur_packet(15, 0), pre_packet(15, 0))
        aligned_inst.rvc := false.B
        aligned_inst.pc := cur_pc - 2.U
      }
    }
    aligned_inst
  }

  output.insts_vec(0) := realign_inst(
    input.bits.pc,
    last_half_inst,
    input.bits.inst_packet(0),
    is_occupied(-1),
    is_occupied(0)
  )

  output.insts_vec(1) := realign_inst(
    input.bits.pc + 2.U,
    input.bits.inst_packet(0),
    input.bits.inst_packet(1),
    is_occupied(0),
    is_occupied(1)
  )

  output.insts_vec(2) := realign_inst(
    input.bits.pc + 4.U,
    input.bits.inst_packet(1),
    input.bits.inst_packet(2),
    is_occupied(1),
    is_occupied(2)
  )

  output.insts_vec(3) := realign_inst(
    input.bits.pc + 6.U,
    input.bits.inst_packet(2),
    input.bits.inst_packet(3),
    is_occupied(2),
    is_occupied(3)
  )

  input.ready := true.B
}

object gen_verilog extends App {

  GenVerilogHelper(new InstReAlign())
}
