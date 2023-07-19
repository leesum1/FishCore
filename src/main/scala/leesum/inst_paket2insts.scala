package leesum

import Chisel.Cat
import chisel3._
import chisel3.util.Decoupled
import chisel3.stage.ChiselStage

// TODO: make RspPacket configurable
class RspPacket extends Bundle {
  // config
  val pc_width = 32
  val payload_width = 64

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
  val insts = Output(Vec(4, UInt(32.W)))
  val insts_valid_mask = Output(Vec(4, Bool()))
  val insts_rvc_mask = Output(Vec(4, Bool()))
  val insts_pc = Output(Vec(4, UInt(32.W)))
}

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
            output.insts(i) := RiscvTools.expand_rvc(
              input.bits.inst_packet(i)
            )
            output.insts_valid_mask(i) := true.B
            output.insts_rvc_mask(i) := true.B
            output.insts_pc(i) := input.bits.pc
          }.otherwise { // 32 位
            output.insts(i) := Cat(
              input.bits.inst_packet(i)(15, 0),
              last_half_inst
            )
            output.insts_valid_mask(i) := true.B
            output.insts_rvc_mask(i) := false.B
            output.insts_pc(i) := input.bits.pc - 2.U
          }
        } else if (i == 3) {
          when(is_occupyied(i - 1)) { // 16 位
            output.insts(i) := RiscvTools.expand_rvc(
              input.bits.inst_packet(i)
            )
            output.insts_valid_mask(i) := true.B
            output.insts_rvc_mask(i) := true.B
            output.insts_pc(i) := input.bits.pc + (i * 2).U
          }.otherwise { // 32 位
            output.insts(i) := Cat(
              input.bits.inst_packet(i)(15, 0),
              input.bits.inst_packet(i - 1)(15, 0)
            )
            output.insts_valid_mask(i) := true.B
            output.insts_rvc_mask(i) := false.B
            output.insts_pc(i) := input.bits.pc + ((i - 1) * 2).U
          }
        } else {
          when(is_occupyied(i - 1)) { // 16 位
            output.insts(i) := RiscvTools.expand_rvc(
              input.bits.inst_packet(i)
            )
            output.insts_valid_mask(i) := true.B
            output.insts_rvc_mask(i) := true.B
            output.insts_pc(i) := input.bits.pc + (i * 2).U

          }.otherwise { // 32 位
            output.insts(i) := Cat(
              input.bits.inst_packet(i)(15, 0),
              input.bits.inst_packet(i - 1)(15, 0)
            )
            output.insts_valid_mask(i) := true.B
            output.insts_rvc_mask(i) := false.B
            output.insts_pc(i) := input.bits.pc + ((i - 1) * 2).U
          }
        }
      }
    }.otherwise {
      output.insts_valid_mask(i) := false.B
      output.insts_rvc_mask(i) := false.B
      output.insts(i) := 2222.U(32.W)
      output.insts_pc(i) := 4.U
    }
  }.otherwise {
    output.insts_valid_mask := VecInit(Seq.fill(4)(false.B))
    output.insts_rvc_mask := VecInit(Seq.fill(4)(false.B))
    output.insts := VecInit(Seq.fill(4)(1111.U(32.W)))
    output.insts_pc := VecInit(Seq.fill(4)(0.U(32.W)))
  }

  input.ready := true.B
}

object gen_verilog extends App {
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new RspPacket2Insts(),
      Array("--target-dir", verilogDir)
    )
}
