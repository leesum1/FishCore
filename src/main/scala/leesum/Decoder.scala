package leesum

import chisel3.util.{Cat, Decoupled, ListLookup, MuxLookup}
import chisel3._
import leesum.RVinst._

class InstDecoder extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new FetchEntry()))

    val out = Decoupled(new ScoreBoardEntry())
  })

  private val decode_sig_seq: Seq[Element] =
    ListLookup(
      io.in.bits.inst,
      RVinst.inst_default,
      i_common_map ++ i64_map ++ m_map ++ m64_map ++ a_map ++ zicsr_map ++ privilege_map
    )
  val decode_sigs = Wire(new DecoderSignals())

//  val inst_default = {
//    List(
//      false.B, // 0-> valid
//      FuType.None, // 1-> function unit
//      FuOP.None, // 2-> alu operation
//      false.B, // 3-> is rv32
//      InstType.R, // 4-> inst type
//      false.B, // 5-> need rs1
//      false.B, // 6-> need rs2
//      false.B, // 7-> need rd
//      false.B, // 8-> need imm
//      false.B, // 9-> need pc
//      false.B // 10-> need immz
//    )
//  }
  // -------------------
  // get decode signals
  // -------------------
  decode_sigs.inst := io.in.bits.inst
  decode_sigs.inst_pc := io.in.bits.pc
  decode_sigs.inst_rvc := io.in.bits.is_rvc
  // TODO: check valid signal?
  decode_sigs.inst_valid := decode_sig_seq(0)
  decode_sigs.fu_type := decode_sig_seq(1)
  decode_sigs.fu_op := decode_sig_seq(2)
  decode_sigs.is_rv32 := decode_sig_seq(3)
  decode_sigs.inst_type := decode_sig_seq(4)
  decode_sigs.need_rs1 := decode_sig_seq(5)
  decode_sigs.need_rs2 := decode_sig_seq(6)
  decode_sigs.need_rd := decode_sig_seq(7)
  decode_sigs.need_imm := decode_sig_seq(8)
  decode_sigs.need_pc := decode_sig_seq(9)
  decode_sigs.need_immz := decode_sig_seq(10)

  val scoreboard_entry = Wire(new ScoreBoardEntry())

  val inst_base = new InstBase(io.in.bits.inst)

  scoreboard_entry.inst := Mux(
    decode_sigs.inst_rvc,
    Cat(0.U(16.W), io.in.bits.inst_c),
    io.in.bits.inst
  )
  scoreboard_entry.pc := decode_sigs.inst_pc
  scoreboard_entry.is_rvc := decode_sigs.inst_rvc
  scoreboard_entry.is_rv32 := decode_sigs.is_rv32
  // -------------------------------
  // scoreboard operand information
  // -------------------------------
  scoreboard_entry.rs1_addr := Mux(decode_sigs.need_rs1, inst_base.rs1, 0.U)
  scoreboard_entry.rs2_addr := Mux(decode_sigs.need_rs2, inst_base.rs2, 0.U)
  scoreboard_entry.rd_addr := Mux(decode_sigs.need_rd, inst_base.rd, 0.U)
  scoreboard_entry.use_pc := decode_sigs.need_pc
  scoreboard_entry.use_imm := decode_sigs.need_imm
  scoreboard_entry.use_immz := decode_sigs.need_immz

  scoreboard_entry.fu_op := decode_sigs.fu_op
  scoreboard_entry.fu_type := decode_sigs.fu_type

  scoreboard_entry.lsu_io_space := false.B

  val imm = MuxLookup(
    decode_sigs.inst_type.asUInt,
    0.U
  )(
    Seq(
      InstType.I.asUInt -> inst_base.imm_i,
      InstType.S.asUInt -> inst_base.imm_s,
      InstType.B.asUInt -> inst_base.imm_b,
      InstType.U.asUInt -> inst_base.imm_u,
      InstType.J.asUInt -> inst_base.imm_j
    )
  )
  // use result to store imm
  scoreboard_entry.result := Mux(decode_sigs.need_imm, imm, 0.U)
  scoreboard_entry.result_valid := false.B

  // -----------------------------------
  // scoreboard exception information
  // -----------------------------------

  when(io.in.bits.exception.valid) {
    // exception happened in fetch stage
    scoreboard_entry.exception := io.in.bits.exception
  }.elsewhen(decode_sigs.fu_op === FuOP.Ebreak) {
    // ebreak
    scoreboard_entry.exception.valid := true.B
    scoreboard_entry.exception.cause := ExceptionCause.breakpoint
    scoreboard_entry.exception.tval := io.in.bits.pc
  }.elsewhen(decode_sigs.fu_op === FuOP.Ecall) {
    // ecall
    scoreboard_entry.exception.valid := true.B
    // temporary cause, the real cause will be set in commit stage
    scoreboard_entry.exception.cause := ExceptionCause.unknown
    scoreboard_entry.exception.tval := 0.U
  }.elsewhen(!decode_sigs.inst_valid) {
    // exception happened in decode stage
    scoreboard_entry.exception.valid := true.B
    scoreboard_entry.exception.cause := ExceptionCause.illegal_instruction
    scoreboard_entry.exception.tval := Mux(
      decode_sigs.inst_rvc,
      Cat(0.U(16.W), io.in.bits.inst_c),
      io.in.bits.inst
    )
  }.otherwise {
    // no exception
    scoreboard_entry.exception.valid := false.B
    scoreboard_entry.exception.cause := DontCare
    scoreboard_entry.exception.tval := 0.U
  }

  // TODO: refactor me!!!
  val exception_valid = scoreboard_entry.exception.valid

  scoreboard_entry.complete := exception_valid || FuOP.is_xret(
    scoreboard_entry.fu_op
  ) || FuOP.is_fence(scoreboard_entry.fu_op)

  // ------------------------------------------
  //  scoreboard branch predictor information
  // ------------------------------------------
  scoreboard_entry.bp := io.in.bits.bp

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  io.out.bits := scoreboard_entry

  // -------------------
  // assert
  // -------------------
  assert(
    !(scoreboard_entry.use_immz && scoreboard_entry.use_imm),
    "use_immz and use_imm should not be true at the same time"
  )

}

object gen_verilog4 extends App {
  GenVerilogHelper(new InstDecoder())

}
