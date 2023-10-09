package leesum
import chisel3._
import chisel3.util.{Decoupled, MuxLookup}

class FuBranchReq extends Bundle {
  val fu_op = FuOP()
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
  def wb_valid = !is_miss_predict
}

class FuBranch extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new FuBranchReq))
    val out = Decoupled(new FuBranchResp)
  })

  // TODO: need optimize, use alu to calculate vaddr
  val pc_rs1 = io.in.bits.pc + io.in.bits.rs1
  val pc_imm = io.in.bits.pc + io.in.bits.imm
  val rs1_imm = io.in.bits.rs1 + io.in.bits.imm
  val ra_target = io.in.bits.pc + Mux(io.in.bits.is_rvc, 2.U, 4.U)

  val taraget_pc = MuxLookup(
    io.in.bits.fu_op.asUInt,
    0.U
  )(
    Seq(
      FuOP.BrJalr.asUInt -> rs1_imm,
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
      FuOP.BrBeq.asUInt -> (io.in.bits.rs1 === io.in.bits.rs2),
      FuOP.BrBne.asUInt -> (io.in.bits.rs1 =/= io.in.bits.rs2),
      FuOP.BrBlt.asUInt -> (io.in.bits.rs1.asSInt < io.in.bits.rs2.asSInt),
      FuOP.BrBge.asUInt -> (io.in.bits.rs1.asSInt >= io.in.bits.rs2.asSInt),
      FuOP.BrBltu.asUInt -> (io.in.bits.rs1 < io.in.bits.rs2),
      FuOP.BrBgeu.asUInt -> (io.in.bits.rs1 >= io.in.bits.rs2)
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
