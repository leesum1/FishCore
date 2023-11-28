package leesum

import chisel3.util.{BitPat, Cat, Decoupled, ListLookup, MuxLookup}
import chisel3._
import chisel3.util.experimental.decode.{TruthTable, decoder}
import leesum.RVinst._

/** Decode risk-v instruction to [ScoreBoardEntry], only support 32-bit
  * instruction
  * @param use_decode
  *   if use_decode is true, use chisel3.util.experimental.decode to decode
  *   instruction, otherwise use ListLookup
  */
class InstDecoder(use_decode: Boolean = true) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new FetchEntry()))
    val out = Decoupled(new ScoreBoardEntry())
  })

  val isa_collection =
    i_common_map ++ i64_map ++ m_map ++ m64_map ++ a_map ++ zicsr_map ++ privilege_map

  val decode_sigs =
    if (use_decode) {
      def to_bitpat(
          vec: List[UInt]
      ) = {
        vec.map(BitPat(_)).reduce((_: BitPat) ## (_: BitPat))
      }
      val decode_bitpat = isa_collection.map(x => (x._1, to_bitpat(x._2)))
      val inst_table = TruthTable(decode_bitpat, to_bitpat(inst_default))
      decoder(io.in.bits.inst, inst_table).asTypeOf(new DecoderSignals())
    } else {
      ListLookup(
        io.in.bits.inst,
        RVinst.inst_default,
        isa_collection
      ).reduce((_: UInt) ## (_: UInt)).asTypeOf(new DecoderSignals())
    }

  val scoreboard_entry = Wire(new ScoreBoardEntry())

  val inst_base = new InstBase(io.in.bits.inst)

  scoreboard_entry.inst := Mux(
    io.in.bits.is_rvc,
    Cat(0.U(16.W), io.in.bits.inst_c),
    io.in.bits.inst
  )

  scoreboard_entry.pc := io.in.bits.pc
  scoreboard_entry.is_rvc := io.in.bits.is_rvc
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
  }.elsewhen(!decode_sigs.valid) {
    // exception happened in decode stage
    scoreboard_entry.exception.valid := true.B
    scoreboard_entry.exception.cause := ExceptionCause.illegal_instruction
    scoreboard_entry.exception.tval := Mux(
      io.in.bits.is_rvc,
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

  val need_complete_insts = VecInit(
    Seq(
      FuOP.Mret.asUInt,
      FuOP.Sret.asUInt,
      FuOP.Fence.asUInt,
      FuOP.FenceI.asUInt,
      FuOP.SFenceVMA.asUInt,
      FuOP.WFI.asUInt
    )
  )
  scoreboard_entry.complete := exception_valid || need_complete_insts.contains(
    decode_sigs.fu_op.asUInt
  )

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
    "use_zimm and use_imm should not be true at the same time"
  )

}

object gen_decode_verilog extends App {
  GenVerilogHelper(new InstDecoder(use_decode = true))
}
