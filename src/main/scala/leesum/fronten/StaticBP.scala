package leesum.fronten

import chisel3._
import chisel3.util.experimental.decode.{TruthTable, decoder}
import chisel3.util.{BitPat, Enum, Mux1H, MuxCase, PopCount}
import leesum.Utils.DecoderHelper
import leesum.{BpEntry, BpType, GenVerilogHelper, InstBase, Instructions}

class StaticBP extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val pc = Input(UInt(64.W))
    val inst = Input(UInt(32.W))
    val bp = Output(new BpEntry)
  })

  val isNone :: isJalr :: isBranch :: isJal :: Nil = Enum(4)

  val mapping =
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
  val pre_decode = DecoderHelper(io.inst, isNone, mapping)

  val inst = new InstBase(io.inst)

  // negative should jump
  val branch_take =
    (pre_decode === isBranch) & inst.imm_b(63) === true.B
  val jal_take = (pre_decode === isJal) & true.B
  val jalr_take = (pre_decode === isJalr) & false.B // can't get rs1

  val branch_target = io.pc + inst.imm_b
  val jal_target = io.pc + inst.imm_j
  val jalr_target = 0.U

  val predict_pc = Mux1H(
    Array(
      branch_take -> branch_target,
      jal_take -> jal_target,
      jalr_take -> jalr_target
    )
  )

  io.bp.bp_type := MuxCase(
    BpType.None,
    Array(
      (pre_decode === isJalr) -> BpType.Jalr,
      (pre_decode === isBranch) -> BpType.Branch,
      (pre_decode === isJal) -> BpType.Jal
    )
  )
  io.bp.predict_pc := predict_pc
  io.bp.is_taken := Seq(branch_take, jal_take, jalr_take).reduce(
    _ | _
  ) & io.valid
  io.bp.is_miss_predict := false.B // init value

  // ---------------------
  // assert
  // ---------------------

  assert(
    PopCount(Seq(branch_take, jal_take, jalr_take)) <= 1.U,
    "only one branch can be taken"
  )

}

object gen_static_bp_verilog extends App {
  GenVerilogHelper(new StaticBP)
}
