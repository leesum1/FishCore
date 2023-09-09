package leesum

import chisel3._
import chisel3.util.{Cat, Decoupled}
import circt.stage.ChiselStage

// TODO: make RspPacket configurable
class RspPacket extends Bundle {
  // config
  private val pc_width = 32
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
  def last_isnt_packet: UInt = {
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
class RspPacket2Insts extends Module {
  val input = IO(Flipped(Decoupled(new RspPacket)))

  val output = IO(
    new InstsItem
  )

  val last_half_inst = RegInit(0.U(16.W));
  val last_half_valid = RegInit(false.B)

  def is_occupyied(idx: Int): Bool = {
    val ret = idx match {
      case -1 => !last_half_valid;
      case 0 => {
        val pre_inst_is_32bits =
          ((!is_occupyied(idx - 1)) & (!RiscvTools.is_rvc(
            Cat(
              input.bits.inst_packet(idx)(15, 0),
              last_half_inst
            )
          )));
        val current_is_rvc = ((is_occupyied(idx - 1)) & RiscvTools.is_rvc(
          input.bits.inst_packet(idx)
        ));

        pre_inst_is_32bits || current_is_rvc
      };
      case _ => {
        val pre_inst_is_32bits =
          ((!is_occupyied(idx - 1)) & (!RiscvTools.is_rvc(
            input.bits.inst_packet(idx - 1)
          )));
        val current_is_rvc = ((is_occupyied(idx - 1)) & RiscvTools.is_rvc(
          input.bits.inst_packet(idx)
        ));

        pre_inst_is_32bits || current_is_rvc
      }
    }

    if (idx < 0) {
      ret.suggestName(s"ocuppyed_neg_${idx.abs}")
    } else {
      ret.suggestName(s"ocuppyed_${idx}")
    }
    ret
  }

  // buffer last half inst
  when(input.fire) {
    last_half_inst := input.bits.inst_packet(3)
    when(is_occupyied(3)) {
      last_half_valid := false.B
    }.otherwise {
      last_half_valid := true.B
    }
  }

  when(input.fire) {
    for (i <- 0 until (input.bits.max_inst_packet)) {
      when(is_occupyied(i)) {
        if (i == 0) { // 16 位
          when(is_occupyied(i - 1)) {
            output.insts_vec(i).inst := RiscvTools.expand_rvc(
              input.bits.inst_packet(i)
            )
            output.insts_valid_mask(i.U) := true.B
            output.insts_rvc_mask(i.U) := true.B
            output.insts_pc(i.U) := input.bits.pc
          }.otherwise { // 32 位
            output.insts(i.U) := Cat(
              input.bits.inst_packet(i)(15, 0),
              last_half_inst
            )
            output.insts_valid_mask(i.U) := true.B
            output.insts_rvc_mask(i.U) := false.B
            output.insts_pc(i.U) := input.bits.pc - 2.U
          }
        } else if (i == 3) {
          when(is_occupyied(i - 1)) { // 16 位
            output.insts(i.U) := RiscvTools.expand_rvc(
              input.bits.inst_packet(i)
            )
            output.insts_valid_mask(i.U) := true.B
            output.insts_rvc_mask(i.U) := true.B
            output.insts_pc(i.U) := input.bits.pc + (i * 2).U
          }.otherwise { // 32 位
            output.insts(i.U) := Cat(
              input.bits.inst_packet(i)(15, 0),
              input.bits.inst_packet(i - 1)(15, 0)
            )
            output.insts_valid_mask(i.U) := true.B
            output.insts_rvc_mask(i.U) := false.B
            output.insts_pc(i.U) := input.bits.pc + ((i - 1) * 2).U
          }
        } else {
          when(is_occupyied(i - 1)) { // 16 位
            output.insts(i.U) := RiscvTools.expand_rvc(
              input.bits.inst_packet(i)
            )
            output.insts_valid_mask(i.U) := true.B
            output.insts_rvc_mask(i.U) := true.B
            output.insts_pc(i.U) := input.bits.pc + (i * 2).U

          }.otherwise { // 32 位
            output.insts(i.U) := Cat(
              input.bits.inst_packet(i)(15, 0),
              input.bits.inst_packet(i - 1)(15, 0)
            )
            output.insts_valid_mask(i.U) := true.B
            output.insts_rvc_mask(i.U) := false.B
            output.insts_pc(i.U) := input.bits.pc + ((i - 1) * 2).U
          }
        }
      }
    }.otherwise {
      output.insts_valid_mask(i.U) := false.B
      output.insts_rvc_mask(i.U) := false.B
      output.insts(i.U) := 0.U(32.W)
      output.insts_pc(i.U) := 0.U
    }
  }.otherwise {
    output.clear()
  }

  input.ready := true.B
}

object gen_verilog extends App {

  GenVerilogHelper(new RspPacket2Insts())
}
