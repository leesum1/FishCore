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
      val fu_alu_wb_port = Flipped(Decoupled(new AluResp))
      // branch
      val fu_branch_wb_port = Flipped(Decoupled(new FuBranchResp))
      // lsu_port
      val fu_lsu_wb_port = Flipped(Decoupled(new LSUResp))
      //  TODO: mul_div
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
        io.fu_alu_wb_port.fire,
        io.fu_branch_wb_port.fire,
        io.fu_lsu_wb_port.fire,
        io.fu_mul_div_valid
      )
    )
    val fu_id = VecInit(
      Seq(
        io.fu_alu_wb_port.bits.trans_id,
        io.fu_branch_wb_port.bits.trans_id,
        io.fu_lsu_wb_port.bits.trans_id,
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

  // TODO: use complete signal?
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

  // no exception for alu
  def alu_write_back(alu_resp: AluResp, en: Bool): Unit = {
    when(en) {
      val rob_idx = alu_resp.trans_id
      rob.content(rob_idx).bits.complete := true.B
      rob.content(rob_idx).bits.result := alu_resp.res
      rob.content(rob_idx).bits.result_valid := true.B

      assert(
        rob.random_access(rob_idx).valid,
        "rob entry must be valid"
      )
    }
  }

  // exception for agu
  def lsu_write_back(lsu_resp: LSUResp, en: Bool): Unit = {
    when(en) {
      val rob_idx = lsu_resp.trans_id
      when(lsu_resp.exception.valid) {
        // 1. exception happened in AGU
        rob.content(rob_idx).bits.complete := true.B
        rob.content(rob_idx).bits.exception := lsu_resp.exception
      }.elsewhen(lsu_resp.is_mmio) {
        // 2. detect mmio access in AGU
        rob.content(rob_idx).bits.complete := false.B
        rob.content(rob_idx).bits.lsu_io_space := lsu_resp.is_mmio
      }.otherwise {
        // 3. normal write-back in LoadQueue
        rob.content(rob_idx).bits.complete := true.B
        rob.content(rob_idx).bits.result := lsu_resp.wb_data
        rob.content(rob_idx).bits.result_valid := true.B
      }
      assert(
        rob.random_access(rob_idx).valid,
        "rob entry must be valid"
      )
    }
  }

  // TODO: exception for branch? rvc?
  def branch_write_back(branch_resp: FuBranchResp, en: Bool): Unit = {
    when(en) {
      val rob_idx = branch_resp.trans_id
      rob.content(rob_idx).bits.complete := true.B
      rob.content(rob_idx).bits.result := branch_resp.wb_data
      rob.content(rob_idx).bits.result_valid := branch_resp.wb_valid
      rob
        .content(rob_idx)
        .bits
        .bp
        .is_miss_predict := branch_resp.is_miss_predict
      rob.content(rob_idx).bits.bp.predict_pc := branch_resp.redirect_pc
      assert(
        rob.random_access(rob_idx).valid,
        "rob entry must be valid"
      )
    }
  }

  io.fu_lsu_wb_port.ready := true.B
  io.fu_alu_wb_port.ready := true.B
  io.fu_branch_wb_port.ready := true.B
  lsu_write_back(io.fu_lsu_wb_port.bits, io.fu_lsu_wb_port.fire)
  alu_write_back(io.fu_alu_wb_port.bits, io.fu_alu_wb_port.fire)
  branch_write_back(io.fu_branch_wb_port.bits, io.fu_branch_wb_port.fire)

  // TODO: NOT IMPLEMENTED
  when(io.fu_mul_div_valid) {
    rob.content(io.fu_mul_div_id).bits.complete := true.B
    rob.content(io.fu_mul_div_id).bits.result := io.fu_mul_div_wb
    rob.content(io.fu_mul_div_id).bits.result_valid := io.fu_mul_div_wb_valid
    rob.content(io.fu_mul_div_id).bits.exception := io.fu_mul_div_exception
  }

  // ---------------------
  // rd_occupied_gpr logic
  // ---------------------

  val rd_occupied_gpr = VecInit(Seq.tabulate(32) { idx =>
    val scb_entry = rob.random_access(idx.U)
    Mux(scb_entry.valid, scb_entry.bits.fu_type, FuType.None)
  })
  // x0 always be None
  rd_occupied_gpr(0) := FuType.None

  io.rd_occupied_gpr := rd_occupied_gpr

  // ----------------------
  // bypass logic
  // ----------------------
  val fu_valid = VecInit(
    Seq(
      io.fu_alu_wb_port.fire,
      io.fu_branch_wb_port.fire,
      io.fu_lsu_wb_port.fire,
      io.fu_mul_div_valid
    )
  )
  val fu_id = VecInit(
    Seq(
      io.fu_alu_wb_port.bits.trans_id,
      io.fu_branch_wb_port.bits.trans_id,
      io.fu_lsu_wb_port.bits.trans_id,
      io.fu_mul_div_id
    )
  )
  val fu_result = VecInit(
    Seq(
      io.fu_alu_wb_port.bits.res,
      io.fu_branch_wb_port.bits.wb_data,
      io.fu_lsu_wb_port.bits.wb_data,
      io.fu_mul_div_wb
    )
  )
  val fu_result_valid = VecInit(
    Seq(
      io.fu_alu_wb_port.fire,
      io.fu_branch_wb_port.bits.wb_valid,
      io.fu_lsu_wb_port.bits.wb_data_valid,
      io.fu_mul_div_wb_valid
    )
  )

  val fwd_valid_seq = Wire(
    Vec(
      num_push_ports,
      (new Bundle {
        val rs1_fwd_valid = Vec(entries + fu_id.length, Bool())
        val rs2_fwd_valid = Vec(entries + fu_id.length, Bool())
      })
    )
  )
  // the first entries of rs_fwd_data is the result of FU
  // the rest entries of rs_fwd_data is the result of SBE
  val fwd_data_vec = VecInit(Seq.tabulate(entries + fu_id.length) { idx =>
    if (idx < fu_id.length) {
      fu_result(idx)
    } else {
      val sbe_idx = idx - fu_id.length
      val sbe_data = rob.random_access(sbe_idx.U).bits
      sbe_data.result
    }
  })

  def check_rs_fwd(rs_addr: UInt): Vec[Bool] = {
    val fwd_valid = VecInit(Seq.fill(entries + fu_id.length)(false.B))
    0.until(fu_id.length)
      .foreach(i => {
        when(
          fu_valid(i) && fu_result_valid(i) && (rob
            .random_access(fu_id(i))
            .bits
            .rd_addr === rs_addr)
        ) {
          fwd_valid(i) := true.B
        }
      })

    0.until(entries)
      .foreach(i => {
        val rob_entry = rob.random_access(i.U)
        when(
          rob_entry.valid && rob_entry.bits
            .rd_data_valid() && rob_entry.bits.rd_addr === rs_addr
        ) {
          fwd_valid(i + fu_id.length) := true.B
        }
      })
    assert(
      PopCount(fwd_valid) <= 1.U,
      "fwd_valid must be less than or equal to 1"
    )
    fwd_valid
  }

  require(fwd_valid_seq.length == io.operand_bypass.length)

  fwd_valid_seq
    .zip(io.operand_bypass)
    .foreach({ case (fwd_valid, bypass) =>
      fwd_valid.rs1_fwd_valid := check_rs_fwd(bypass.rs1_addr)
      fwd_valid.rs2_fwd_valid := check_rs_fwd(bypass.rs2_addr)
    })

  io.operand_bypass
    .zip(fwd_valid_seq)
    .foreach({ case (bypass, fwd_valid) =>
      bypass.rs1_fwd_valid := fwd_valid.rs1_fwd_valid.reduce(_ || _) && !bypass
        .rs1_is_x0()
      bypass.rs2_fwd_valid := fwd_valid.rs2_fwd_valid.reduce(_ || _) && !bypass
        .rs2_is_x0()

      bypass.rs1_data := Mux1H(fwd_valid.rs1_fwd_valid, fwd_data_vec)
      bypass.rs2_data := Mux1H(fwd_valid.rs2_fwd_valid, fwd_data_vec)

    })
}

object gen_ScoreBoard_verilog extends App {
  GenVerilogHelper(new ScoreBoard(8, 2, 2))
}
