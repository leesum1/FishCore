package leesum

import chisel3._
import chisel3.util.{Decoupled, Mux1H, PopCount, isPow2, log2Ceil}

class OperandByPassIO extends Bundle {
  val rs1_addr = Input(UInt(5.W))
  val rs1_data = Output(UInt(64.W))
  val rs1_fwd_valid = Output(Bool())

  val rs2_addr = Input(UInt(5.W))
  val rs2_data = Output(UInt(64.W))
  val rs2_fwd_valid = Output(Bool())

  def rs1_is_x0(): Bool = rs1_addr === 0.U
  def rs2_is_x0(): Bool = rs2_addr === 0.U
}

class ScoreBoard(
    val entries: Int,
    val num_push_ports: Int,
    val num_pop_ports: Int
) extends Module() {
  require(
    entries > 0,
    "ScoreBoard must have non-zero number of entries"
  )
  require(
    isPow2(entries),
    "ScoreBoard must have power-of-2 number of entries"
  )

  val genType = new ScoreBoardEntry()

  val io = IO(
    new Bundle {
      val push_ports = Vec(num_push_ports, Flipped(Decoupled(genType)))
      val pop_ports = Vec(num_pop_ports, Decoupled(genType))
      val flush = Input(Bool())
      // trans_id of the current push data(scoreboard-entry)
      val push_trans_id =
        Output(Vec(num_push_ports, UInt(log2Ceil(entries).W)))

      val operand_bypass = Vec(num_push_ports, new OperandByPassIO())
      val rd_occupied_gpr = Vec(32, FuType())

      // write-back port
      // alu
      val fu_alu_valid = Input(Bool())
      val fu_alu_id = Input(UInt(log2Ceil(entries).W))
      val fu_alu_wb = Input(UInt(64.W))
      val fu_alu_wb_valid = Input(Bool())
      val fu_alu_exception =
        Input(new ExceptionEntry()) // use for fetch exception
      // branch
      val fu_branch_valid = Input(Bool())
      val fu_branch_id = Input(UInt(log2Ceil(entries).W))
      val fu_branch_wb = Input(UInt(64.W))
      val fu_branch_wb_valid = Input(Bool())
      val fu_branch_miss_predict = Input(Bool())
      val fu_branch_redirect_pc = Input(UInt(64.W))
      // lsu_port
      val fu_lsu_valid = Input(Bool())
      val fu_lsu_id = Input(UInt(log2Ceil(entries).W))
      val fu_lsu_wb = Input(UInt(64.W))
      val fu_lsu_wb_valid = Input(Bool())
      val fu_lsu_is_mmio = Input(Bool()) // io space load,such as uart
      val fu_lsu_exception = Input(new ExceptionEntry())

      // mul_div
      val fu_mul_div_valid = Input(Bool())
      val fu_mul_div_id = Input(UInt(log2Ceil(entries).W))
      val fu_mul_div_wb = Input(UInt(64.W))
      val fu_mul_div_wb_valid = Input(Bool())
      val fu_mul_div_exception = Input(new ExceptionEntry())
    }
  )

  // if Write-back ID conflict, then return false
  def checkWritebackConflict(): Bool = {
    val fu_valid = VecInit(
      Seq(
        io.fu_alu_valid,
        io.fu_branch_valid,
        io.fu_lsu_valid,
        io.fu_mul_div_valid
      )
    )
    val fu_id = VecInit(
      Seq(
        io.fu_alu_id,
        io.fu_branch_id,
        io.fu_lsu_id,
        io.fu_mul_div_id
      )
    )
    val is_distinct = WireInit(true.B)
    for (i <- 0 until fu_id.length) {
      for (j <- i + 1 until fu_id.length) {
        when(fu_valid(i) && fu_valid(j)) {
          when(fu_id(i) === fu_id(j)) {
            is_distinct := false.B
          }
        }
      }
    }
    is_distinct
  }

  assert(
    checkWritebackConflict(),
    "writeback conflict"
  )

  // ---------------------------
  //  internal memory
  // ---------------------------

  val rob = new MultiPortValidFIFO(
    gen = genType,
    entries,
    name = "rob",
    num_push_ports,
    num_pop_ports
  )

  // ---------------------------
  //  push pop ports logic
  // ---------------------------

  rob.push_pop_flush_cond_multi_port(
    push_cond = VecInit(io.push_ports.map(_.fire)),
    pop_cond = VecInit(io.pop_ports.map(_.fire)),
    flush_cond = io.flush,
    entry = VecInit(io.push_ports.map(_.bits))
  )

  val pop_peek = rob.peek()
  pop_peek.zipWithIndex.foreach({ case (pop, i) =>
    io.pop_ports(i).bits := pop.bits
    io.pop_ports(i).valid := pop.valid
  })

  0.until(num_push_ports)
    .foreach(i =>
      io.push_ports(i).ready := !rob.random_access(rob.push_ptr + i.U).valid
    )
  // ---------------------------
  // allocate trans_id for push
  // ---------------------------
  0.until(num_push_ports)
    .foreach(i => {
      io.push_trans_id(i) := rob.push_ptr + i.U
    })

  // -------------------------
  // write-back ports logic
  // -------------------------
  when(io.fu_alu_valid) {
    rob.content(io.fu_alu_id).bits.complete := true.B
    rob.content(io.fu_alu_id).bits.result := io.fu_alu_wb
    rob.content(io.fu_alu_id).bits.result_valid := io.fu_alu_wb_valid
    rob.content(io.fu_alu_id).bits.exception := io.fu_alu_exception
  }
  when(io.fu_branch_valid) {
    rob.content(io.fu_branch_id).bits.complete := true.B
    rob.content(io.fu_branch_id).bits.result := io.fu_branch_wb
    rob.content(io.fu_branch_id).bits.result_valid := io.fu_branch_wb_valid
    rob
      .content(io.fu_branch_id)
      .bits
      .bp
      .is_miss_predict := io.fu_branch_miss_predict
    rob.content(io.fu_branch_id).bits.bp.predict_pc := io.fu_branch_redirect_pc
  }
  when(io.fu_lsu_valid) {
//    rob.content(io.fu_lsu_id).bits.complete := true.B
//    rob.content(io.fu_lsu_id).bits.result := io.fu_load_wb
//    rob.content(io.fu_lsu_id).bits.result_valid := io.fu_load_wb_valid
//    rob.content(io.fu_lsu_id).bits.exception := io.fu_load_exception
//    rob.content(io.fu_lsu_id).bits.lsu_io_space := io.fu_load_io_space
  }

  when(io.fu_mul_div_valid) {
    rob.content(io.fu_mul_div_id).bits.complete := true.B
    rob.content(io.fu_mul_div_id).bits.result := io.fu_mul_div_wb
    rob.content(io.fu_mul_div_id).bits.result_valid := io.fu_mul_div_wb_valid
    rob.content(io.fu_mul_div_id).bits.exception := io.fu_mul_div_exception
  }

  // ---------------------
  // rd_occupied_gpr logic
  // ---------------------

  val rd_occupied_gpr = VecInit(Seq.fill(32)(FuType.None))

  0.until(entries)
    .foreach(idx => {
      val scb_entry = rob.random_access(idx.U).bits
      rd_occupied_gpr(scb_entry.rd_addr) := scb_entry.fu_type
    })
  // x0 always be None
  rd_occupied_gpr(0) := FuType.None

  io.rd_occupied_gpr := rd_occupied_gpr

  // ----------------------
  // bypass logic
  // ----------------------
  val fu_valid = VecInit(
    Seq(
      io.fu_alu_valid,
      io.fu_branch_valid,
      io.fu_lsu_valid,
      io.fu_mul_div_valid
    )
  )
  val fu_id = VecInit(
    Seq(
      io.fu_alu_id,
      io.fu_branch_id,
      io.fu_lsu_id,
      io.fu_mul_div_id
    )
  )
  val fu_result = VecInit(
    Seq(
      io.fu_alu_wb,
      io.fu_branch_wb,
      io.fu_lsu_wb,
      io.fu_mul_div_wb
    )
  )
  val fu_result_valid = VecInit(
    Seq(
      io.fu_alu_wb_valid,
      io.fu_branch_wb_valid,
      io.fu_lsu_wb_valid,
      io.fu_mul_div_wb_valid
    )
  )
  // fwd_req 中最多只有一个有效？
  val op1_rs1_fwd_req = Wire(Vec(entries + fu_id.length, Bool()))
  val op1_rs2_fwd_req = Wire(Vec(entries + fu_id.length, Bool()))
  val op2_rs1_fwd_req = Wire(Vec(entries + fu_id.length, Bool()))
  val op2_rs2_fwd_req = Wire(Vec(entries + fu_id.length, Bool()))
  val rs_fwd_data = Wire(Vec(entries + fu_id.length, UInt(64.W)))

  assert(
    PopCount(op1_rs1_fwd_req) <= 1.U,
    "fwd_req must be less than or equal to 1"
  )
  assert(
    PopCount(op1_rs2_fwd_req) <= 1.U,
    "fwd_req must be less than or equal to 1"
  )
  assert(
    PopCount(op2_rs1_fwd_req) <= 1.U,
    "fwd_req must be less than or equal to 1"
  )
  assert(
    PopCount(op2_rs2_fwd_req) <= 1.U,
    "fwd_req must be less than or equal to 1"
  )

  0.until(op1_rs1_fwd_req.length)
    .foreach(i => {
      op1_rs1_fwd_req(i) := false.B
      op1_rs2_fwd_req(i) := false.B
      op2_rs1_fwd_req(i) := false.B
      op2_rs2_fwd_req(i) := false.B
    })

  0.until(fu_id.length)
    .foreach(i => {
      rs_fwd_data(i) := fu_result(i)
      when(
        fu_valid(i) && fu_result_valid(i) && (rob
          .random_access(fu_id(i))
          .bits
          .rd_addr === io
          .operand_bypass(0)
          .rs1_addr)
      ) {
        op1_rs1_fwd_req(i) := true.B
      }
      when(
        fu_valid(i) && fu_result_valid(i) && rob
          .random_access(fu_id(i))
          .bits
          .rd_addr === io
          .operand_bypass(0)
          .rs2_addr
      ) {
        op1_rs2_fwd_req(i) := true.B
      }
      when(
        fu_valid(i) && fu_result_valid(i) && rob
          .random_access(fu_id(i))
          .bits
          .rd_addr === io
          .operand_bypass(1)
          .rs1_addr
      ) {
        op2_rs1_fwd_req(i) := true.B
      }
      when(
        fu_valid(i) && fu_result_valid(i) && rob
          .random_access(fu_id(i))
          .bits
          .rd_addr === io
          .operand_bypass(1)
          .rs1_addr
      ) {
        op2_rs1_fwd_req(i) := true.B
      }
      when(
        fu_valid(i) && fu_result_valid(i) && rob
          .random_access(fu_id(i))
          .bits
          .rd_addr === io
          .operand_bypass(1)
          .rs2_addr
      ) {
        op2_rs2_fwd_req(i) := true.B
      }

    })

  fu_id.length
    .until(entries + fu_id.length)
    .foreach(i => {
      val sbe_idx = i - fu_id.length
      val sbe_data = rob.random_access(sbe_idx.U).bits
      rs_fwd_data(i) := sbe_data.result
      when(
        sbe_data.rd_data_valid() && sbe_data.rd_addr === io
          .operand_bypass(0)
          .rs1_addr
      ) {
        op1_rs1_fwd_req(i) := true.B
      }
      when(
        sbe_data.rd_data_valid() && sbe_data.rd_addr === io
          .operand_bypass(0)
          .rs2_addr
      ) {
        op1_rs2_fwd_req(i) := true.B
      }
      when(
        sbe_data.rd_data_valid() && sbe_data.rd_addr === io
          .operand_bypass(1)
          .rs1_addr
      ) {
        op2_rs1_fwd_req(i) := true.B
      }
      when(
        sbe_data.rd_data_valid() && sbe_data.rd_addr === io
          .operand_bypass(1)
          .rs2_addr
      ) {
        op2_rs2_fwd_req(i) := true.B
      }
    })

  // ------------------------------------------------------------
  //  val op1_rs1_fwd_req = Vec(entries + fu_id.length, Bool())
  //  val op1_rs2_fwd_req = Vec(entries + fu_id.length, Bool())
  //  val op2_rs1_fwd_req = Vec(entries + fu_id.length, Bool())
  //  val op2_rs2_fwd_req = Vec(entries + fu_id.length, Bool())
  //  val rs_fwd_data = Vec(entries + fu_id.length, UInt(64.W))
  // ------------------------------------------------------------

  io.operand_bypass(0).rs1_data := Mux1H(op1_rs1_fwd_req, rs_fwd_data)
  io.operand_bypass(0).rs2_data := Mux1H(op1_rs2_fwd_req, rs_fwd_data)
  io.operand_bypass(1).rs1_data := Mux1H(op2_rs1_fwd_req, rs_fwd_data)
  io.operand_bypass(1).rs2_data := Mux1H(op2_rs2_fwd_req, rs_fwd_data)
  io.operand_bypass(0).rs1_fwd_valid := Mux(
    io.operand_bypass(0).rs1_is_x0(),
    false.B,
    op1_rs1_fwd_req.reduce(_ || _)
  )
  io.operand_bypass(0).rs2_fwd_valid := Mux(
    io.operand_bypass(0).rs2_is_x0(),
    false.B,
    op1_rs2_fwd_req.reduce(_ || _)
  )
  io.operand_bypass(1).rs1_fwd_valid := Mux(
    io.operand_bypass(1).rs1_is_x0(),
    false.B,
    op2_rs1_fwd_req.reduce(_ || _)
  )
  io.operand_bypass(1).rs2_fwd_valid := Mux(
    io.operand_bypass(1).rs2_is_x0(),
    false.B,
    op2_rs2_fwd_req.reduce(_ || _)
  )

}

object gen_ScoreBoard_verilog extends App {
  GenVerilogHelper(new ScoreBoard(8, 2, 2))
}
