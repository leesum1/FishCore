package leesum

import chisel3._
import chisel3.util.{Fill, Mux1H, PopCount, isPow2, log2Ceil}

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
    val num_pushports: Int,
    val num_popports: Int
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
      val push_data = Input(Vec(num_pushports, genType))
      val push_valid = Input(Vec(num_pushports, Bool()))
      val pop_data = Output(Vec(num_popports, genType))
      val pop_valid = Input(Vec(num_popports, Bool()))
      val flush = Input(Bool())
      // fifo status
      val empty = Output(Bool())
      val full = Output(Bool())
      val free_entries = Output(UInt((log2Ceil(entries) + 1).W))
      val occupied_entries = Output(UInt((log2Ceil(entries) + 1).W))
      // id of the current push data(scoreboard-entry)
      val write_ptr_trans_id =
        Output(Vec(num_pushports, UInt(log2Ceil(entries).W)))

      val operand_bypass = Vec(2, new OperandByPassIO())
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
      // load
      val fu_load_valid = Input(Bool())
      val fu_load_id = Input(UInt(log2Ceil(entries).W))
      val fu_load_wb = Input(UInt(64.W))
      val fu_load_wb_valid = Input(Bool())
      val fu_load_io_space = Input(Bool()) // io space load,such as uart
      val fu_load_exception = Input(new ExceptionEntry())
      // store
      val fu_store_valid = Input(Bool())
      val fu_store_id = Input(UInt(log2Ceil(entries).W))
      val fu_store_exception = Input(new ExceptionEntry())
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
        io.fu_load_valid,
        io.fu_store_valid,
        io.fu_mul_div_valid
      )
    )
    val fu_id = VecInit(
      Seq(
        io.fu_alu_id,
        io.fu_branch_id,
        io.fu_load_id,
        io.fu_store_id,
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
    CheckOrder(io.pop_valid),
    "pop_valid must be ordered"
  )
  assert(
    CheckOrder(io.push_valid),
    "push_valid must be ordered"
  )
  assert(
    checkWritebackConflict(),
    "writeback conflict"
  )

  ////////////////////////////
  /// internal memory
  ////////////////////////////
  val ram = Mem(entries, genType)
  val push_ptr = RegInit(0.U(log2Ceil(entries).W))
  val pop_ptr = RegInit(0.U(log2Ceil(entries).W))
  val num_counter = RegInit(0.U((log2Ceil(entries) + 1).W))
  // -------------------------
  // write-back ports logic
  // -------------------------
  when(io.fu_alu_valid) {
    ram(io.fu_alu_id).complete := true.B
    ram(io.fu_alu_id).result := io.fu_alu_wb
    ram(io.fu_alu_id).result_valid := io.fu_alu_wb_valid
    ram(io.fu_alu_id).exception := io.fu_alu_exception
  }
  when(io.fu_branch_valid) {
    ram(io.fu_branch_id).complete := true.B
    ram(io.fu_branch_id).result := io.fu_branch_wb
    ram(io.fu_branch_id).result_valid := io.fu_branch_wb_valid
    ram(io.fu_branch_id).bp.is_miss_predict := io.fu_branch_miss_predict
    ram(io.fu_branch_id).bp.predict_pc := io.fu_branch_redirect_pc
  }
  when(io.fu_load_valid) {
    ram(io.fu_load_id).complete := true.B
    ram(io.fu_load_id).result := io.fu_load_wb
    ram(io.fu_load_id).result_valid := io.fu_load_wb_valid
    ram(io.fu_load_id).exception := io.fu_load_exception
    ram(io.fu_load_id).lsu_io_space := io.fu_load_io_space
  }
  when(io.fu_store_valid) {
    ram(io.fu_store_id).result := 0.U
    ram(io.fu_store_id).result_valid := false.B
    ram(io.fu_store_id).exception := io.fu_store_exception
  }
  when(io.fu_mul_div_valid) {
    ram(io.fu_mul_div_id).complete := true.B
    ram(io.fu_mul_div_id).result := io.fu_mul_div_wb
    ram(io.fu_mul_div_id).result_valid := io.fu_mul_div_wb_valid
    ram(io.fu_mul_div_id).exception := io.fu_mul_div_exception
  }

  num_counter := num_counter - PopCount(
    io.pop_valid
  ) + PopCount(
    io.push_valid
  )
  val empty = num_counter === 0.U
  val full = num_counter === entries.U
  val free_entries = entries.U - num_counter

  io.empty := empty
  io.full := full
  io.free_entries := free_entries
  io.occupied_entries := num_counter

  assert(
    PopCount(io.push_valid) <= free_entries,
    "push_valid must be less than or equal to free_entries"
  )
  assert(
    PopCount(io.pop_valid) <= num_counter,
    "pop_rsp must be less than or equal to num_counter"
  )
  // -----------------
  // push logic
  // ----------------
  0.until(num_pushports)
    .foreach(i => {
      when(io.push_valid(i)) {
        ram(push_ptr + i.U) := io.push_data(i)
      }
    })

  0.until(num_pushports)
    .foreach(i => {
      io.write_ptr_trans_id(i) := push_ptr + i.U
    })

  push_ptr := push_ptr + PopCount(io.push_valid)
  // ------------------
  // pop logic
  // ------------------
  0.until(num_popports)
    .foreach(i => {
      io.pop_data(i) := ram(pop_ptr + i.U)
      ram(pop_ptr + i.U).clear_valid()
    })
  pop_ptr := pop_ptr + PopCount(io.pop_valid)

  // ------------------
  // flush logic
  // ------------------
  when(io.flush) {
    for (i <- 0 until entries) {
      ram(i).clear_valid()
    }
    pop_ptr := 0.U
    push_ptr := 0.U
    num_counter := 0.U
  }

  // ---------------------
  // rd_occupied_gpr logic
  // ---------------------

  val rd_occupied_gpr = VecInit(Seq.fill(32)(FuType.None))

  0.until(entries)
    .foreach(idx => {
      val scb_entry = ram(idx)
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
      io.fu_load_valid,
      io.fu_mul_div_valid
    )
  )
  val fu_id = VecInit(
    Seq(
      io.fu_alu_id,
      io.fu_branch_id,
      io.fu_load_id,
      io.fu_mul_div_id
    )
  )
  val fu_result = VecInit(
    Seq(
      io.fu_alu_wb,
      io.fu_branch_wb,
      io.fu_load_wb,
      io.fu_mul_div_wb
    )
  )
  val fu_result_valid = VecInit(
    Seq(
      io.fu_alu_wb_valid,
      io.fu_branch_wb_valid,
      io.fu_load_wb_valid,
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
        fu_valid(i) && fu_result_valid(i) && (ram(fu_id(i)).rd_addr === io
          .operand_bypass(0)
          .rs1_addr)
      ) {
        op1_rs1_fwd_req(i) := true.B
      }
      when(
        fu_valid(i) && fu_result_valid(i) && ram(fu_id(i)).rd_addr === io
          .operand_bypass(0)
          .rs2_addr
      ) {
        op1_rs2_fwd_req(i) := true.B
      }
      when(
        fu_valid(i) && fu_result_valid(i) && ram(fu_id(i)).rd_addr === io
          .operand_bypass(1)
          .rs1_addr
      ) {
        op2_rs1_fwd_req(i) := true.B
      }
      when(
        fu_valid(i) && fu_result_valid(i) && ram(fu_id(i)).rd_addr === io
          .operand_bypass(1)
          .rs1_addr
      ) {
        op2_rs1_fwd_req(i) := true.B
      }
      when(
        fu_valid(i) && fu_result_valid(i) && ram(fu_id(i)).rd_addr === io
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
      val sbe_data = ram(sbe_idx)
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
