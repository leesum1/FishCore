package leesum
import chisel3._
import chisel3.util.{Decoupled, MuxLookup}
import circt.stage.ChiselStage

class FuBranchIn extends Bundle {
  val fu_op = Input(FuOP())
  val trans_id = Input(UInt(32.W))
  val bp = Input(new BpEntry())
  val is_rvc = Input(Bool())
  val pc = Input(UInt(64.W))
  val imm = Input(UInt(64.W))
  val op_a = Input(UInt(64.W))
  val op_b = Input(UInt(64.W))
}
class FuBranchOut extends Bundle {
  // jal and jalr need write back to register
  val trans_id = Output(UInt(32.W))
  val wb_data = Output(UInt(64.W))
  // if branch is mis-predicted, redirect_pc is the correct pc
  val is_miss_predict = Output(Bool())
  val redirect_pc = Output(UInt(64.W))
}

class FuBranch extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new FuBranchIn))
    val out = Decoupled(new FuBranchOut)
  })

  val pc_rs1 = io.in.bits.pc + io.in.bits.op_a
  val pc_imm = io.in.bits.pc + io.in.bits.imm
  val ra_target = io.in.bits.pc + Mux(io.in.bits.is_rvc, 2.U, 4.U)

  val taraget_pc = MuxLookup(
    io.in.bits.fu_op.asUInt,
    0.U,
    Seq(
      FuOP.BrJalr.asUInt -> pc_rs1,
      FuOP.BrJal.asUInt -> pc_imm,
      FuOP.BrBeq.asUInt -> pc_imm,
      FuOP.BrBne.asUInt -> pc_imm,
      FuOP.BrBlt.asUInt -> pc_imm,
      FuOP.BrBge.asUInt -> pc_imm,
      FuOP.BrBltu.asUInt -> pc_imm,
      FuOP.BrBgeu.asUInt -> pc_imm
    )
  )

  val branch_taken = MuxLookup(
    io.in.bits.fu_op.asUInt,
    false.B
  )(
    Seq(
      FuOP.BrJalr.asUInt -> true.B,
      FuOP.BrJal.asUInt -> true.B,
      FuOP.BrBeq.asUInt -> (io.in.bits.op_a === io.in.bits.op_b),
      FuOP.BrBne.asUInt -> (io.in.bits.op_a =/= io.in.bits.op_b),
      FuOP.BrBlt.asUInt -> (io.in.bits.op_a.asSInt < io.in.bits.op_b.asSInt),
      FuOP.BrBge.asUInt -> (io.in.bits.op_a.asSInt >= io.in.bits.op_b.asSInt),
      FuOP.BrBltu.asUInt -> (io.in.bits.op_a < io.in.bits.op_b),
      FuOP.BrBgeu.asUInt -> (io.in.bits.op_a >= io.in.bits.op_b)
    )
  )

  val is_miss_predict = Wire(Bool())
  when(io.in.bits.bp.is_taken === branch_taken) {
    when(io.in.bits.bp.is_taken) {
      is_miss_predict := Mux(
        io.in.bits.bp.predict_pc === taraget_pc,
        false.B,
        true.B
      )
    }.otherwise {
      is_miss_predict := false.B
    }
  }.otherwise {
    is_miss_predict := true.B
  }

  io.out.bits.is_miss_predict := is_miss_predict
  io.out.bits.redirect_pc := taraget_pc
  io.out.bits.wb_data := ra_target
  io.out.bits.trans_id := io.in.bits.trans_id

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}
object gen_Fubranch extends App {
  GenVerilogHelper(new FuBranch())

}
