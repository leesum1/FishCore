package leesum

import chisel3.DontCare.:=
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.{Bundle, Flipped, Input, Module}
import chisel3.util.{BitPat, Decoupled, ListLookup, MuxLookup}
import chisel3.util.experimental.decode._

class InstDecoder extends Module {
  val io = IO(new Bundle {
    val in = Input(new FetchEntry())

    val out = Output(new ScoreBoardEntry())
  })

  private val decode_sig_seq: Seq[Element] =
    ListLookup(io.in.inst, RVinst.inst_default, RVinst.inst_map)
  val decode_sigs = Wire(new DecoderSignals())
  // -------------------
  // get decode signals
  // -------------------
  decode_sigs.inst := io.in.inst
  decode_sigs.inst_pc := io.in.pc
  decode_sigs.inst_rvc := io.in.is_rvc
  decode_sigs.inst_valid := decode_sig_seq(0)
  decode_sigs.fu_type := decode_sig_seq(1)
  decode_sigs.fu_op := decode_sig_seq(2)
  decode_sigs.sign_ext := decode_sig_seq(3)
  decode_sigs.op_width := decode_sig_seq(4)
  decode_sigs.inst_type := decode_sig_seq(5)
  decode_sigs.need_rs1 := decode_sig_seq(6)
  decode_sigs.need_rs2 := decode_sig_seq(7)
  decode_sigs.need_rd := decode_sig_seq(8)
  decode_sigs.need_imm := decode_sig_seq(9)
  decode_sigs.need_pc := decode_sig_seq(10)
  decode_sigs.need_immz := decode_sig_seq(11)

  val scoreboard_entry = Wire(new ScoreBoardEntry())

  val inst_base = Module(new InstBase(io.in.inst))

  scoreboard_entry.inst := decode_sigs.inst
  scoreboard_entry.pc := decode_sigs.inst_pc
  scoreboard_entry.is_rvc := decode_sigs.inst_rvc
  // ---------------------
  // scoreboard operand information
  // ---------------------
  scoreboard_entry.rs1_addr := Mux(decode_sigs.need_rs1, inst_base.rs1, 0.U)
  scoreboard_entry.rs2_addr := Mux(decode_sigs.need_rs2, inst_base.rs2, 0.U)
  scoreboard_entry.rd_addr := Mux(decode_sigs.need_rd, inst_base.rd, 0.U)
  scoreboard_entry.use_pc := decode_sigs.need_pc;
  scoreboard_entry.use_imm := decode_sigs.need_imm
  scoreboard_entry.use_immz := decode_sigs.need_immz

  scoreboard_entry.fu_op := decode_sigs.fu_op
  scoreboard_entry.fu_type := decode_sigs.fu_type

  // TODO!
  val imm = MuxLookup(
    decode_sigs.inst_type.asUInt,
    0.U,
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

  // -----------------------
  // scoreboard exception information
  // -----------------------
  when(io.in.exception.valid) {
    // exception happened in fetch stage
    scoreboard_entry.exception := io.in.exception
  }.elsewhen(!decode_sigs.inst_valid) {
    // exception happened in decode stage
    scoreboard_entry.exception.valid := true.B
    scoreboard_entry.exception.cause := ExceptionCause.illegal_instruction
    scoreboard_entry.exception.tval := io.in.inst
  }.otherwise {
    // no exception
    scoreboard_entry.exception.valid := false.B
    scoreboard_entry.exception.cause := DontCare
    scoreboard_entry.exception.tval := 0.U
  }
  // -----------------------
  //  scoreboard branch predictor information
  // -----------------------
  scoreboard_entry.bp := io.in.bp

  io.out := scoreboard_entry
}

object gen_verilog4 extends App {
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new InstDecoder(),
      Array(
        "--target-dir",
        verilogDir,
        "--emission-options=disableMemRandomization,disableRegisterRandomization"
      )
    )
}
