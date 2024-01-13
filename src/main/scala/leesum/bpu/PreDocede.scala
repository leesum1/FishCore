package leesum.bpu

import chisel3._
import chisel3.util.{Enum, Mux1H, MuxCase, PopCount}
import leesum.Utils.DecoderHelper
import leesum._

class PreDocode extends Module {
  val io = IO(new Bundle {
    val pc = Input(UInt(64.W))
    val inst = Input(UInt(32.W))
    val jump_target_pc = Output(UInt(64.W)) // valid when is branch or jal
    val jump_type = Output(BpType())

  })

  val isNone :: isJalr :: isBranch :: isJal :: Nil = Enum(4)

  val jump_mapping =
    Seq(
      Instructions.IType("JALR") -> isJalr,
      Instructions.IType("JAL") -> isJal,
      Instructions.IType("BEQ") -> isBranch,
      Instructions.IType("BNE") -> isBranch,
      Instructions.IType("BLT") -> isBranch,
      Instructions.IType("BGE") -> isBranch,
      Instructions.IType("BLTU") -> isBranch,
      Instructions.IType("BGEU") -> isBranch
    )
  val pre_decode = DecoderHelper(io.inst, isNone, jump_mapping)

  val inst = new InstBase(io.inst)

  val branch_target = io.pc + inst.imm_b
  val jal_target = io.pc + inst.imm_j

  // assign output
  io.jump_type := Mux1H(
    Array(
      (pre_decode === isJalr) -> BpType.Jalr,
      (pre_decode === isBranch) -> BpType.Branch,
      (pre_decode === isJal) -> BpType.Jal,
      (pre_decode === isNone) -> BpType.None
    )
  )
  io.jump_target_pc := Mux1H(
    Array(
      (pre_decode === isJalr) -> 0.U,
      (pre_decode === isBranch) -> branch_target,
      (pre_decode === isJal) -> jal_target
    )
  )

}

object gen_PreDocede_verilog extends App {
  GenVerilogHelper(new PreDocode)
}
