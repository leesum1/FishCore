package leesum

import Chisel.Cat
import chisel3._
import chisel3.util.Decoupled

class RspPacket extends Bundle {
  // config
  val pc_with = 32
  val inst_boundary = 16
  val packet_width = 64
  val insts_per_packet = packet_width / inst_boundary

  // port
//  val pc = UInt(pc_with.W)
  val paket_payload = UInt(64.W)

  def inst_boundary(idx: Int): UInt = {
    require(idx >= 0 && idx < insts_per_packet)
    val x = paket_payload(16 * idx + 15, 16 * idx)
    x
  }
}

class RspPacket2Insts extends Module {
  val input = IO(Flipped(Decoupled(new RspPacket)))

  val output = IO(
    new Bundle {
      val insts = Output(Vec(4, UInt(32.W)))
      val insts_mask = Output(Vec(4, Bool()))
      val insts_rvc_mask = Output(Vec(4, Bool()))
    }
  )

  val last_half_inst = RegInit(0.U(16.W));
  val last_half_valid = RegInit(false.B)

  def ocuppyed(idx: Int): Bool = {
    val ret = idx match {
      case -1 => !last_half_valid;
      case 0 =>
        ((!ocuppyed(idx - 1)) & (!RiscvTools.is_rvc(
          Cat(
            input.bits.inst_boundary(idx)(15, 0),
            last_half_inst
          )
        ))) || ((ocuppyed(idx - 1)) & RiscvTools.is_rvc(
          input.bits.inst_boundary(idx)
        ));
      case _ =>
        ((!ocuppyed(idx - 1)) & (!RiscvTools.is_rvc(
          input.bits.inst_boundary(idx - 1)
        ))) || ((ocuppyed(idx - 1)) & RiscvTools.is_rvc(
          input.bits.inst_boundary(idx)
        ));
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
    last_half_inst := input.bits.inst_boundary(3)
    when(ocuppyed(3)) {
      last_half_valid := false.B
    }.otherwise {
      last_half_valid := true.B
    }
  }

  when(input.fire) {
    for (i <- 0 until (input.bits.insts_per_packet)) {
      when(ocuppyed(i)) {
        if (i == 0) { // 16 位
          when(ocuppyed(i - 1)) {
            output.insts(i) := RiscvTools.expand_rvc(
              input.bits.inst_boundary(i)
            )
            output.insts_mask(i) := true.B
            output.insts_rvc_mask(i) := true.B
          }.otherwise { // 32 位
            output.insts(i) := Cat(
              input.bits.inst_boundary(i)(15, 0),
              last_half_inst
            )
            output.insts_mask(i) := true.B
            output.insts_rvc_mask(i) := false.B
          }
        } else if (i == 3) {
          when(ocuppyed(i - 1)) { // 16 位
            output.insts(i) := RiscvTools.expand_rvc(
              input.bits.inst_boundary(i)
            )
            output.insts_mask(i) := true.B
            output.insts_rvc_mask(i) := true.B
          }.otherwise { // 32 位
            output.insts(i) := Cat(
              input.bits.inst_boundary(i)(15, 0),
              input.bits.inst_boundary(i - 1)(15, 0)
            )
            output.insts_mask(i) := true.B
            output.insts_rvc_mask(i) := false.B
          }
        } else {
          when(ocuppyed(i - 1)) { // 16 位
            output.insts(i) := RiscvTools.expand_rvc(
              input.bits.inst_boundary(i)
            )
            output.insts_mask(i) := true.B
            output.insts_rvc_mask(i) := true.B
          }.otherwise { // 32 位
            output.insts(i) := Cat(
              input.bits.inst_boundary(i)(15, 0),
              input.bits.inst_boundary(i - 1)(15, 0)
            )
            output.insts_mask(i) := true.B
            output.insts_rvc_mask(i) := false.B
          }
        }
      }
    }.otherwise {
      output.insts_mask(i) := false.B
      output.insts_rvc_mask(i) := false.B
      output.insts(i) := 2222.U(32.W)
    }
  }.otherwise {
    output.insts_mask := VecInit(Seq.fill(4)(false.B))
    output.insts_rvc_mask := VecInit(Seq.fill(4)(false.B))
    output.insts := VecInit(Seq.fill(4)(1111.U(32.W)))
  }

  // when(input.fire) {
  //   last_half_valid := false.B
  //   for (i <- 0 until (input.bits.insts_per_packet)) {
  //     if (i == 0) {
  //       val (inst_0_0_is_rvc, inst_0_0) = RiscvTools.get_inst(
  //         input.bits.inst_boundary(0),
  //         last_half_inst(15, 0)
  //       )
  //       val (inst_0_1_is_rvc, inst_0_1) = RiscvTools.get_inst(
  //         input.bits.inst_boundary(1),
  //         input.bits.inst_boundary(0)
  //       )
  //       when(!inst_0_0_is_rvc & last_half_valid) {
  //         output.insts(0) := inst_0_0
  //         output.insts_rvc_mask(0) := false.B
  //         output.insts_mask(0) := true.B
  //       }.otherwise {
  //         output.insts(0) := inst_0_1
  //         output.insts_mask(0) := true.B
  //         output.insts_rvc_mask(0) := inst_0_1_is_rvc
  //       }
  //     } else if (i == input.bits.insts_per_packet - 1) {
  //       val (inst_i_is_rvc, inst_i) = RiscvTools.get_inst(
  //         0.U,
  //         input.bits.inst_boundary(i)
  //       )
  //       output.insts(i) := inst_i
  //       // 如果前一条指令为 32 位指令,当前 16 bit 已经被占用
  //       when(!output.insts_rvc_mask(i - 1) & output.insts_mask(i - 1)) {
  //         output.insts_rvc_mask(i) := false.B
  //         output.insts_mask(i) := false.B
  //       } // 如果前一条指令为 16 位指令,当前 16 bit 未被占用
  //         // 如果当前指令位 16 bit 指令,则输出
  //         .elsewhen(
  //           inst_i_is_rvc & output.insts_rvc_mask(i - 1) & output.insts_mask(
  //             i - 1
  //           )
  //         ) {
  //           output.insts_rvc_mask(i) := true.B
  //           output.insts_mask(i) := true.B
  //         } // 如果前一条指令为 16 位指令,当前 16 bit 未被占用
  //         // 如果当前指令位 32 bit ,则无法拼接为 32 bit 指令,保存到寄存器中供下一次使用
  //         .elsewhen(
  //           !inst_i_is_rvc & output.insts_rvc_mask(i - 1) & output.insts_mask(
  //             i - 1
  //           )
  //         ) {
  //           output.insts_rvc_mask(i) := false.B
  //           output.insts_mask(i) := false.B
  //           last_half_valid := true.B
  //         }
  //         .
  //         // 如果前一条指令为 32 位指令,并且当前 16 bit 未被占用
  //         elsewhen(output.insts_mask(i - 2) & !output.insts_rvc_mask(i - 1)) {
  //           when(inst_i_is_rvc) {
  //             output.insts_rvc_mask(i) := true.B
  //             output.insts_mask(i) := true.B
  //           }.otherwise {
  //             output.insts_rvc_mask(i) := false.B
  //             output.insts_mask(i) := false.B
  //             last_half_valid := true.B
  //           }
  //         }
  //         .otherwise {
  //           output.insts_rvc_mask(i) := false.B
  //           output.insts_mask(i) := false.B
  //         }
  //     } else if (i == 1) {
  //       val (inst_i_is_rvc, inst_i) = RiscvTools.get_inst(
  //         input.bits.inst_boundary(i + 1),
  //         input.bits.inst_boundary(i)
  //       )
  //       output.insts(i) := inst_i
  //       // 第一条指令是 跨域的 32 位指令,不会影响当前指令
  //       when(
  //         !output.insts_rvc_mask(i - 1) & output.insts_mask(
  //           i - 1
  //         ) & last_half_valid
  //       ) {
  //         output.insts_rvc_mask(i) := inst_i_is_rvc
  //         output.insts_mask(i) := true.B
  //         // 第一条指令是 16 位指令,不会影响当前指令
  //       }.elsewhen(output.insts_rvc_mask(i - 1)) {
  //         output.insts_rvc_mask(i) := inst_i_is_rvc
  //         output.insts_mask(i) := true.B
  //         // 第一条指令是 32 位指令,会影响当前指令
  //       }.otherwise {
  //         output.insts_rvc_mask(i) := false.B
  //         output.insts_mask(i) := false.B
  //       }
  //     } else {
  //       val (inst_i_is_rvc, inst_i) = RiscvTools.get_inst(
  //         input.bits.inst_boundary(i + 1),
  //         input.bits.inst_boundary(i)
  //       )
  //       output.insts(i) := inst_i
  //       // 前一条指令是 32 位指令,则会影响当前指令
  //       when(!output.insts_rvc_mask(i - 1) & output.insts_mask(i - 1)) {
  //         output.insts_rvc_mask(i) := false.B
  //         output.insts_mask(i) := false.B
  //       } // 前一条指令是 16 位指令,则不会影响当前指令
  //         .elsewhen(output.insts_rvc_mask(i - 1)) {
  //           output.insts_rvc_mask(i) := inst_i_is_rvc
  //           output.insts_mask(i) := true.B
  //         }
  //         .otherwise {
  //           output.insts_rvc_mask(i) := false.B
  //           output.insts_mask(i) := false.B
  //         }
  //     }
  //   }
  // }.otherwise {
  //   output.insts_mask := VecInit(Seq.fill(4)(false.B))
  //   output.insts_rvc_mask := VecInit(Seq.fill(4)(false.B))
  //   output.insts := VecInit(Seq.fill(4)(1111.U(32.W)))
  // }

  input.ready := true.B
}

object gen_verilog extends App {
  emitVerilog(new RspPacket2Insts())
}

//test(new RspPacket2Insts){dut=>{
//
//
//
//}}
