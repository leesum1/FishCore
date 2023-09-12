package leesum
import chisel3._
import chisel3.util._

class IssueFifoPopIO extends Bundle {
  val pop_valid = Output(Vec(2, Bool()))
  val pop_data = Input(Vec(2, new ScoreBoardEntry))
  val occupied_entries = Input(UInt((log2Ceil(8) + 1).W))
  def inst_valid_seq(): Vec[Bool] = {
    val valid_seq = Wire(Vec(2, Bool()))
    valid_seq(0) := occupied_entries >= 1.U
    valid_seq(1) := occupied_entries >= 2.U
    valid_seq
  }
}

class IssueStageScbIO extends Bundle {
  val push_data = Output(Vec(2, new ScoreBoardEntry()))
  val push_valid = Output(Vec(2, Bool()))
  val free_entries = Input(UInt((log2Ceil(8) + 1).W))
  val write_ptr_trans_id =
    Input(Vec(2, UInt(log2Ceil(8).W)))
  val operand_bypass = Vec(2, Flipped(new OperandByPassIO()))
  val rd_occupied_gpr = Input(Vec(32, FuType()))
}

class FuIO extends Bundle {

  val alu_count = 2
  val branch_count = 1
  val lsu_count = 1
  val mul_div_count = 1
  val csr_count = 1

  val alu_0 = Decoupled(new AluIn())
  val alu_1 = Decoupled(new AluIn())
  val branch_0 = Decoupled(new FuBranchIn())
  val csr_0 = Decoupled(UInt(32.W))
  val lsu_0 = Decoupled(UInt(32.W))
  val mul_0 = Decoupled(UInt(32.W))
  val div_0 = Decoupled(UInt(32.W))
}

class IssueStage extends Module {
  val io = IO(new Bundle {
    val issuefifo_pop_port = new IssueFifoPopIO()
    val scb_port = new IssueStageScbIO()
    val fu_port = new FuIO()
  })
  // data from issue fifo
  val fifo_valid_seq = MuxCase(
    VecInit(false.B, false.B),
    Seq(
      (io.issuefifo_pop_port.occupied_entries >= 2.U) -> VecInit(
        true.B,
        true.B
      ),
      (io.issuefifo_pop_port.occupied_entries === 1.U) -> VecInit(
        true.B,
        false.B
      )
    )
  )
  val fifo_data_seq = io.issuefifo_pop_port.pop_data

  // ----------------
  // bypass logic
  // ----------------
  io.scb_port.operand_bypass(0).rs1_addr := fifo_data_seq(0).rs1_addr
  io.scb_port.operand_bypass(0).rs2_addr := fifo_data_seq(0).rs2_addr
  io.scb_port.operand_bypass(1).rs1_addr := fifo_data_seq(1).rs1_addr
  io.scb_port.operand_bypass(1).rs2_addr := fifo_data_seq(1).rs2_addr

  // ---------------------------------
  //  stall logic
  // ---------------------------------

  val rs1_regfile = WireInit(0.U(64.W))
  val rs2_regfile = WireInit(0.U(64.W))

  val inst_stall = VecInit(false.B, false.B)
  val inst_op_a = Wire(Vec(2, UInt(64.W)))
  val inst_op_b = Wire(Vec(2, UInt(64.W)))

  // when fwd_valid is false, and the rs1/rs2 is occupied by other inst, stall
  // when fwd_valid is false, and the rs1/rs2 is not occupied by other inst, read from regfile
  // when fwd_valid is true, and the rs1/rs2 is occupied by other inst, bypass rd to rs1/rs2
  // when fwd_valid is true, and the rs1/rs2 is not occupied by other inst, error!!!!!!!!!
  def check_operands(
      bypass: OperandByPassIO,
      scb: ScoreBoardEntry,
      stall: Bool
  ): Unit = {

    when(
      bypass.rs1_fwd_valid && io.scb_port.rd_occupied_gpr(
        scb.rs1_addr
      ) =/= FuType.None
    ) {
      assert(false.B, "rs1 read operands logic error")
    }
    when(
      bypass.rs2_fwd_valid && io.scb_port.rd_occupied_gpr(
        scb.rs2_addr
      ) =/= FuType.None
    ) {
      assert(false.B, "rs2 read operands logic error")
    }

    when(
      !bypass.rs1_fwd_valid && io.scb_port.rd_occupied_gpr(
        scb.rs1_addr
      ) =/= FuType.None
    ) {
      stall := true.B
    }
    when(
      !bypass.rs2_fwd_valid && io.scb_port.rd_occupied_gpr(
        scb.rs2_addr
      ) =/= FuType.None
    ) {
      stall := true.B
    }
  }

  // Check WAW hazard
  // if the rd is occupied by other inst on the scoreboard, stall
  def check_waw(
      scb: ScoreBoardEntry,
      stall: Bool
  ) = {
    when(
      io.scb_port.rd_occupied_gpr(scb.rd_addr) =/= FuType.None
    ) {
      stall := true.B
    }
  }
  check_waw(fifo_data_seq(0), inst_stall(0))
  check_waw(fifo_data_seq(1), inst_stall(1))
  check_operands(io.scb_port.operand_bypass(0), fifo_data_seq(0), inst_stall(0))
  check_operands(io.scb_port.operand_bypass(1), fifo_data_seq(1), inst_stall(1))

  // RAW hazard between inst 0 and inst 1
  when(
    (fifo_data_seq(0).rd_addr === fifo_data_seq(1).rs1_addr || fifo_data_seq(
      0
    ).rd_addr === fifo_data_seq(1).rs2_addr) && fifo_data_seq(0).rd_addr =/= 0.U
  ) {
    inst_stall(1) := true.B
  }
  // WAW hazard between inst 0 and inst 1
  when(
    (fifo_data_seq(0).rd_addr === fifo_data_seq(1).rd_addr) && fifo_data_seq(
      0
    ).rd_addr =/= 0.U
  ) {
    inst_stall(1) := true.B
  }

  // -----------------------
  // read operands logic
  // -----------------------

  // TODO: not all inst need rs1 or rs2, some inst may need imm or pc
  def read_operands(
      bypass: OperandByPassIO,
      scb: ScoreBoardEntry,
      op_a: UInt,
      op_b: UInt
  ): Unit = {
    assert(
      PopCount(
        VecInit(
          bypass.rs1_fwd_valid,
          scb.use_pc,
          scb.use_imm
        )
      ) <= 1.U,
      "rs1 read operands logic error"
    )
    assert(
      PopCount(
        VecInit(
          bypass.rs2_fwd_valid,
          scb.use_imm
        )
      ) <= 1.U,
      "rs2 read operands logic error"
    )
    when(bypass.rs1_fwd_valid) {
      op_a := bypass.rs1_data
    }.elsewhen(scb.use_pc) {
      op_a := scb.pc
    }.elsewhen(scb.use_imm) {
      op_a := scb.result
    }.otherwise {
      op_a := rs1_regfile
    }
    when(bypass.rs2_fwd_valid) {
      op_b := bypass.rs2_data
    }.elsewhen(scb.use_imm) {
      op_b := scb.result
    }.otherwise {
      op_b := rs2_regfile
    }
  }
  read_operands(
    io.scb_port.operand_bypass(0),
    fifo_data_seq(0),
    inst_op_a(0),
    inst_op_b(0)
  )
  read_operands(
    io.scb_port.operand_bypass(1),
    fifo_data_seq(1),
    inst_op_a(1),
    inst_op_b(1)
  )
  // -----------------------
  // issue logic
  // -----------------------
  val allow_issue = VecInit(false.B, false.B)
  val inst_valid_seq = io.issuefifo_pop_port.inst_valid_seq()

  // When inst is valid, begin to issue logic. there are three conditions:
  // 1. Inst is not stall (pass operand check)
  // 2. Inst is stall, but exception happened
  // 3. Inst is stall, but inst does not need operands, such as ecall, ebreak
  when(inst_valid_seq(0)) {
    when(!inst_stall(0)) {
      allow_issue(0) := true.B
    }.elsewhen(fifo_data_seq(0).exception.valid) {
      allow_issue(0) := true.B
    }.elsewhen(fifo_data_seq(0).fu_type === FuType.None) {
      allow_issue(0) := true.B
    }
  }.otherwise {
    allow_issue(0) := false.B
  }

  when(inst_valid_seq(1)) {
    when(!inst_stall(1)) {
      allow_issue(1) := true.B
    }.elsewhen(fifo_data_seq(1).exception.valid) {
      allow_issue(1) := true.B
    }.elsewhen(fifo_data_seq(1).fu_type === FuType.None) {
      allow_issue(1) := true.B
    }
  }.otherwise {
    allow_issue(0) := false.B
  }
  // -----------------------
  // Check fu hazard
  // -----------------------
  val fu_seq = VecInit(fifo_data_seq(0).fu_type, fifo_data_seq(1).fu_type)
  val fu_hazard = VecInit(false.B, false.B)

  fu_hazard(0) := MuxLookup(fu_seq(0), false.B)(
    Seq(
      FuType.Alu -> !io.fu_port.alu_0.ready,
      FuType.Br -> !io.fu_port.branch_0.ready,
      FuType.Lsu -> !io.fu_port.lsu_0.ready,
      FuType.Mul -> !io.fu_port.mul_0.ready,
      FuType.Div -> !io.fu_port.div_0.ready,
      FuType.Csr -> !io.fu_port.csr_0.ready
    )
  )
  fu_hazard(1) := MuxLookup(fu_seq(1), false.B)(
    Seq(
      FuType.Alu -> !io.fu_port.alu_1.ready,
      FuType.Br -> !io.fu_port.branch_0.ready,
      FuType.Lsu -> !io.fu_port.lsu_0.ready,
      FuType.Mul -> !io.fu_port.mul_0.ready,
      FuType.Div -> !io.fu_port.div_0.ready,
      FuType.Csr -> !io.fu_port.csr_0.ready
    )
  )
  // when inst0 and inst1 use the same fu, and is not FuType.None
  // 1. if the fu is alu, no hazard, because we have two alu unit
  // 2. if the fu is not alu, inst1 should stall
  when(fu_seq(0) === fu_seq(1) && fu_seq(0) =/= FuType.None) {
    when(fu_seq(0) =/= FuType.Alu) {
      fu_hazard(1) := true.B
    }
  }
  // -----------------------
  // dispatch to fu logic
  // -----------------------
  // TODO: how to do when exception happened?
  val dispatch_seq = allow_issue
    .zip(fu_hazard)
    .map { case (allow, hazard) =>
      allow && !hazard
    }
    .toVector

  val scb_push_ready = VecInit(
    io.scb_port.free_entries >= 1.U,
    io.scb_port.free_entries >= 2.U
  )

  io.scb_port.push_valid := dispatch_seq.zip(scb_push_ready).map {
    case (dispatch, push_ready) =>
      dispatch && push_ready
  }
  io.scb_port.push_data := fifo_data_seq

  io.issuefifo_pop_port.pop_valid := io.scb_port.push_valid

  // -------------------
  // alu logic
  // -------------------
  def dispatch_to_alu(
      alu: DecoupledIO[AluIn],
      scb: ScoreBoardEntry,
      alu_allow_dispatch: Bool,
      op_a: UInt,
      op_b: UInt,
      trans_id: UInt
  ): Unit = {
    require(op_a.getWidth == 64 && op_b.getWidth == 64)
    val alu_op = alu.bits.convert_fuop2aluop(scb.fu_op)
    alu.valid := alu_allow_dispatch && alu_op =/= AluOP.None
    alu.bits.a := op_a
    alu.bits.b := op_b
    alu.bits.op := alu_op
    alu.bits.is_rv32 := scb.is_rv32
    alu.bits.trans_id := trans_id
  }
  dispatch_to_alu(
    io.fu_port.alu_0,
    fifo_data_seq(0),
    dispatch_seq(0) && fifo_data_seq(0).fu_type === FuType.Alu,
    inst_op_a(0),
    inst_op_b(0),
    io.scb_port.write_ptr_trans_id(0)
  )
  dispatch_to_alu(
    io.fu_port.alu_1,
    fifo_data_seq(1),
    dispatch_seq(1) && fifo_data_seq(1).fu_type === FuType.Alu,
    inst_op_a(1),
    inst_op_b(1),
    io.scb_port.write_ptr_trans_id(1)
  )
  // ------------------
  // br logic
  // ------------------
  val br_allow_dispatch = VecInit(
    dispatch_seq(0) && fifo_data_seq(0).fu_type === FuType.Br,
    dispatch_seq(1) && fifo_data_seq(1).fu_type === FuType.Br
  )

  assert(
    PopCount(br_allow_dispatch) <= 1.U,
    "branch unit can only dispatch one inst at one cycle"
  )
  // TODO: performance optimization?
  val br_scb = PriorityMux(br_allow_dispatch, fifo_data_seq)
  val br_trans_id =
    PriorityMux(br_allow_dispatch, io.scb_port.write_ptr_trans_id)
  val br_op_a = PriorityMux(br_allow_dispatch, inst_op_a)
  val br_op_b = PriorityMux(br_allow_dispatch, inst_op_b)

  def dispatch_to_br(
      br: DecoupledIO[FuBranchIn],
      scb: ScoreBoardEntry,
      br_allow_dispatch: Bool,
      op_a: UInt,
      op_b: UInt,
      trans_id: UInt
  ): Unit = {
    br.valid := br_allow_dispatch
    br.bits.fu_op := scb.fu_op
    br.bits.op_a := op_a
    br.bits.op_b := op_b
    br.bits.pc := scb.pc
    br.bits.imm := scb.result
    br.bits.is_rvc := scb.is_rvc
    br.bits.trans_id := trans_id
    br.bits.bp := scb.bp
  }

  dispatch_to_br(
    io.fu_port.branch_0,
    br_scb,
    br_allow_dispatch.reduce(_ || _),
    br_op_a,
    br_op_b,
    br_trans_id
  )
  // -------------------
  // lsu logic
  // -------------------
  val lsu_allow_dispatch = VecInit(
    dispatch_seq(0) && fifo_data_seq(0).fu_type === FuType.Lsu,
    dispatch_seq(1) && fifo_data_seq(1).fu_type === FuType.Lsu
  )

  assert(
    PopCount(lsu_allow_dispatch) <= 1.U,
    "load-store unit can only dispatch one inst at one cycle"
  )
  // TODO: performance optimization?
  val lsu_scb = PriorityMux(lsu_allow_dispatch, fifo_data_seq)
  val lsu_trans_id =
    PriorityMux(lsu_allow_dispatch, io.scb_port.write_ptr_trans_id)
  val lsu_op_a = PriorityMux(lsu_allow_dispatch, inst_op_a)
  val lsu_op_b = PriorityMux(lsu_allow_dispatch, inst_op_b)

  io.fu_port.lsu_0.valid := lsu_allow_dispatch.reduce(_ || _)
  io.fu_port.lsu_0.bits := 0.U
  // ----------------------
  // csr logic
  // ----------------------
  val csr_allow_dispatch = VecInit(
    dispatch_seq(0) && fifo_data_seq(0).fu_type === FuType.Csr,
    dispatch_seq(1) && fifo_data_seq(1).fu_type === FuType.Csr
  )
  assert(
    PopCount(csr_allow_dispatch) <= 1.U,
    "csr unit can only dispatch one inst at one cycle"
  )
  // TODO: performance optimization?
  val csr_scb = PriorityMux(csr_allow_dispatch, fifo_data_seq)
  val csr_trans_id =
    PriorityMux(csr_allow_dispatch, io.scb_port.write_ptr_trans_id)
  val csr_op_a = PriorityMux(csr_allow_dispatch, inst_op_a)
  val csr_op_b = PriorityMux(csr_allow_dispatch, inst_op_b)

  io.fu_port.csr_0.valid := csr_allow_dispatch.reduce(_ || _)
  io.fu_port.csr_0.bits := 0.U
  // -------------------
  // mul logic
  // -------------------
  val mul_allow_dispatch = VecInit(
    dispatch_seq(0) && fifo_data_seq(0).fu_type === FuType.Mul,
    dispatch_seq(1) && fifo_data_seq(1).fu_type === FuType.Mul
  )
  assert(
    PopCount(mul_allow_dispatch) <= 1.U,
    "mul unit can only dispatch one inst at one cycle"
  )
  // TODO: performance optimization?
  val mul_scb = PriorityMux(mul_allow_dispatch, fifo_data_seq)
  val mul_trans_id =
    PriorityMux(mul_allow_dispatch, io.scb_port.write_ptr_trans_id)
  val mul_op_a = PriorityMux(mul_allow_dispatch, inst_op_a)
  val mul_op_b = PriorityMux(mul_allow_dispatch, inst_op_b)
  io.fu_port.mul_0.valid := mul_allow_dispatch.reduce(_ || _)
  io.fu_port.mul_0.bits := 0.U
  // ------------------
  // div logic
  // ------------------
  val div_allow_dispatch = VecInit(
    dispatch_seq(0) && fifo_data_seq(0).fu_type === FuType.Div,
    dispatch_seq(1) && fifo_data_seq(1).fu_type === FuType.Div
  )
  assert(
    PopCount(div_allow_dispatch) <= 1.U,
    "div unit can only dispatch one inst at one cycle"
  )
  // TODO: performance optimization?
  val div_scb = PriorityMux(div_allow_dispatch, fifo_data_seq)
  val div_trans_id =
    PriorityMux(div_allow_dispatch, io.scb_port.write_ptr_trans_id)
  val div_op_a = PriorityMux(div_allow_dispatch, inst_op_a)
  val div_op_b = PriorityMux(div_allow_dispatch, inst_op_b)
  io.fu_port.div_0.valid := div_allow_dispatch.reduce(_ || _)
  io.fu_port.div_0.bits := 0.U

}

object gen_IssueStage_verilog extends App {
  GenVerilogHelper(new IssueStage())
}
