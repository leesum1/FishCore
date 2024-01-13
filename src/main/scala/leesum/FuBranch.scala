package leesum
import chisel3._
import chisel3.util.{Cat, Decoupled, MuxLookup}
import leesum.axi4.SkidBufferWithFLush

class FuBranchReq extends Bundle {
  val fu_op = UInt(FuOP.width.W)
  val trans_id = UInt(32.W)
  val bp = new BpEntry()
  val is_rvc = Bool()
  val pc = UInt(64.W)
  val imm = UInt(64.W)
  val rs1 = UInt(64.W)
  val rs2 = UInt(64.W)
}
class FuBranchResp extends Bundle {
  // jal and jalr need write back to register
  val trans_id = UInt(32.W)
  val wb_data = UInt(64.W)
  // if branch is mis-predicted, redirect_pc is the correct pc
  val is_miss_predict = Bool()
  val redirect_pc = UInt(64.W)
  val exception = new ExceptionEntry()
  val branch_type = BpType()
  def wb_valid = !is_miss_predict && !exception.valid
}

class FuBranch(rvc_en: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new FuBranchReq))
    val out = Decoupled(new FuBranchResp)
    val flush = Input(Bool())
  })

  val pc_imm = io.in.bits.pc + io.in.bits.imm
  val rs1_imm = io.in.bits.rs1 + io.in.bits.imm
  val pc_inc = io.in.bits.pc + Mux(io.in.bits.is_rvc, 2.U, 4.U)

  val target_pc = MuxLookup(
    io.in.bits.fu_op.asUInt,
    0.U
  )(
    Seq(
      FuOP.BrJalr.asUInt -> Cat(rs1_imm(63, 1), 0.U(1.W)),
      FuOP.BrJal.asUInt -> Cat(pc_imm(63, 1), 0.U(1.W)),
      FuOP.BrBeq.asUInt -> pc_imm,
      FuOP.BrBne.asUInt -> pc_imm,
      FuOP.BrBlt.asUInt -> pc_imm,
      FuOP.BrBge.asUInt -> pc_imm,
      FuOP.BrBltu.asUInt -> pc_imm,
      FuOP.BrBgeu.asUInt -> pc_imm
    )
  )
  dontTouch(target_pc)

  val branch_taken = MuxLookup(
    io.in.bits.fu_op.asUInt,
    false.B
  )(
    Seq(
      FuOP.BrJalr.asUInt -> true.B,
      FuOP.BrJal.asUInt -> true.B,
      FuOP.BrBeq.asUInt -> (io.in.bits.rs1 === io.in.bits.rs2),
      FuOP.BrBne.asUInt -> (io.in.bits.rs1 =/= io.in.bits.rs2),
      FuOP.BrBlt.asUInt -> (io.in.bits.rs1.asSInt < io.in.bits.rs2.asSInt),
      FuOP.BrBge.asUInt -> (io.in.bits.rs1.asSInt >= io.in.bits.rs2.asSInt),
      FuOP.BrBltu.asUInt -> (io.in.bits.rs1 < io.in.bits.rs2),
      FuOP.BrBgeu.asUInt -> (io.in.bits.rs1 >= io.in.bits.rs2)
    )
  )

  dontTouch(branch_taken)

  val is_miss_predict = (io.in.bits.bp.is_taken =/= branch_taken) ||
    (io.in.bits.bp.is_taken && io.in.bits.bp.predict_pc =/= target_pc)

  val redirect_pc = Wire(UInt(64.W))

  dontTouch(redirect_pc)
  when(io.in.bits.bp.is_taken =/= branch_taken) {
    redirect_pc := Mux(branch_taken, target_pc, pc_inc)
  }.elsewhen(branch_taken && io.in.bits.bp.predict_pc =/= target_pc) {
    redirect_pc := target_pc
  }.otherwise {
    redirect_pc := target_pc
  }

  dontTouch(is_miss_predict)

  val exception_valid = if (rvc_en) {
    false.B
  } else {
    target_pc(1)
  }

  val br_resp = Wire(Decoupled(new FuBranchResp))
  br_resp.valid := io.in.valid
  io.in.ready := br_resp.ready

  br_resp.bits.is_miss_predict := is_miss_predict

  // used in commit stage
  br_resp.bits.redirect_pc := redirect_pc
  br_resp.bits.wb_data := pc_inc
  br_resp.bits.trans_id := io.in.bits.trans_id
  br_resp.bits.exception.valid := exception_valid
  br_resp.bits.exception.cause := ExceptionCause.misaligned_fetch
  br_resp.bits.exception.tval := target_pc
  br_resp.bits.branch_type := io.in.bits.bp.bp_type

  when(io.in.valid) {
    assert(io.in.bits.bp.bp_type =/= BpType.None)
  }

  SkidBufferWithFLush(
    br_resp,
    io.out,
    io.flush,
    CUT_VALID = true,
    CUT_READY = false
  )
}
object gen_Fubranch extends App {
  GenVerilogHelper(new FuBranch())
}
