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
      //  mul_div_port
      val fu_mul_div_wb_port = Flipped(Decoupled(new FuMulDivResp))
    }
  )

  // if Write-back ID conflict, then return false
  def check_write_back_conflict(): Bool = {
    val fu_valid = VecInit(
      Seq(
        io.fu_alu_wb_port.fire,
        io.fu_branch_wb_port.fire,
        io.fu_lsu_wb_port.fire,
        io.fu_mul_div_wb_port.fire
      )
    )
    val fu_id = VecInit(
      Seq(
        io.fu_alu_wb_port.bits.trans_id,
        io.fu_branch_wb_port.bits.trans_id,
        io.fu_lsu_wb_port.bits.trans_id,
        io.fu_mul_div_wb_port.bits.trans_id
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
    check_write_back_conflict(),
    "writeback conflict"
  )

  // ---------------------------
  //  internal fifo
  // ---------------------------

  val rob = new MultiPortValidFIFO2(
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

  // TODO: performance???
  0.until(num_push_ports)
    .foreach(i => io.push_ports(i).ready := rob.free_entries > i.U)

  // ---------------------------
  // allocate trans_id for push
  // ---------------------------
  0.until(num_push_ports)
    .foreach(i => {
      io.push_trans_id(i) := rob.push_ptr_seq(i)
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
        rob.create_read_port(rob_idx).valid,
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
        rob.create_read_port(rob_idx).valid,
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
        rob.create_read_port(rob_idx).valid,
        "rob entry must be valid"
      )
    }
  }

  // no exception for mul_div
  def mul_div_write_back(
      mul_div_resp: FuMulDivResp,
      en: Bool
  ): Unit = {
    when(en) {
      val rob_idx = mul_div_resp.trans_id
      rob.content(rob_idx).bits.complete := true.B
      rob.content(rob_idx).bits.result := mul_div_resp.data
      rob.content(rob_idx).bits.result_valid := true.B

      assert(
        rob.create_read_port(rob_idx).valid,
        "rob entry must be valid"
      )
    }
  }

  io.fu_alu_wb_port.ready := true.B
  io.fu_branch_wb_port.ready := true.B
  io.fu_lsu_wb_port.ready := true.B
  io.fu_mul_div_wb_port.ready := true.B

  alu_write_back(io.fu_alu_wb_port.bits, io.fu_alu_wb_port.fire)
  mul_div_write_back(io.fu_mul_div_wb_port.bits, io.fu_mul_div_wb_port.fire)
  lsu_write_back(io.fu_lsu_wb_port.bits, io.fu_lsu_wb_port.fire)
  branch_write_back(io.fu_branch_wb_port.bits, io.fu_branch_wb_port.fire)

  // ---------------------
  // rd_occupied_gpr logic
  // ---------------------

  val rd_occupied_gpr = WireInit(VecInit(Seq.fill(32)(FuType.None)))

  rob.content.foreach(entry => {
    when(entry.valid) {
      rd_occupied_gpr(entry.bits.rd_addr) := entry.bits.fu_type
    }
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
      io.fu_mul_div_wb_port.fire
    )
  )
  val fu_id = VecInit(
    Seq(
      io.fu_alu_wb_port.bits.trans_id,
      io.fu_branch_wb_port.bits.trans_id,
      io.fu_lsu_wb_port.bits.trans_id,
      io.fu_mul_div_wb_port.bits.trans_id
    )
  )
  val fu_result = VecInit(
    Seq(
      io.fu_alu_wb_port.bits.res,
      io.fu_branch_wb_port.bits.wb_data,
      io.fu_lsu_wb_port.bits.wb_data,
      io.fu_mul_div_wb_port.bits.data
    )
  )
  val fu_result_valid = VecInit(
    Seq(
      io.fu_alu_wb_port.fire,
      io.fu_branch_wb_port.bits.wb_valid,
      io.fu_lsu_wb_port.bits.wb_data_valid,
      io.fu_mul_div_wb_port.fire
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
      val sbe_data = rob.create_read_port(sbe_idx.U).bits
      sbe_data.result
    }
  })

  def check_rs_fwd(rs_addr: UInt): Vec[Bool] = {
    val fwd_valid = VecInit(Seq.fill(entries + fu_id.length)(false.B))
    0.until(fu_id.length)
      .foreach(i => {
        when(
          fu_valid(i) && fu_result_valid(i) && (rob
            .create_read_port(fu_id(i))
            .bits
            .rd_addr === rs_addr) && rs_addr =/= 0.U
        ) {
          fwd_valid(i) := true.B
        }
      })

    0.until(entries)
      .foreach(i => {
        val rob_entry = rob.create_read_port(i.U)
        when(
          rob_entry.valid && rob_entry.bits
            .rd_data_valid() && rob_entry.bits.rd_addr === rs_addr && rs_addr =/= 0.U
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
      bypass.rs1_fwd_valid := fwd_valid.rs1_fwd_valid.reduce(_ || _)
      bypass.rs2_fwd_valid := fwd_valid.rs2_fwd_valid.reduce(_ || _)

      bypass.rs1_data := Mux1H(fwd_valid.rs1_fwd_valid, fwd_data_vec)
      bypass.rs2_data := Mux1H(fwd_valid.rs2_fwd_valid, fwd_data_vec)

    })

  // ----------------------
  // assert
  // ----------------------

  rob.content.foreach(entry => {

    when(entry.valid) {
      when(entry.bits.result_valid) {
        assert(entry.bits.complete, "rob entry must be complete")
      }

      when(entry.bits.exception.valid) {
        assert(entry.bits.complete, "rob entry must be complete")
      }

      assert(
        !(entry.bits.exception.valid && entry.bits.result_valid),
        "result and exception can't be valid at the same time"
      )
    }
  })

  val rd_waw = WireInit(false.B)

  for (
    i <- 0 until entries;
    j <- i + 1 until entries
  ) {
    val entry_i = rob.create_read_port(i.U)
    val entry_j = rob.create_read_port(j.U)
    val is_x0 = entry_i.bits.rd_addr === 0.U && entry_j.bits.rd_addr === 0.U
    when(
      entry_i.valid && entry_j.valid && entry_i.bits.rd_addr === entry_j.bits.rd_addr && !is_x0
    ) {
      rd_waw := true.B
    }
  }

  assert(
    !rd_waw,
    "rd_waw error!"
  )

}

object gen_ScoreBoard_verilog extends App {
  GenVerilogHelper(new ScoreBoard(8, 2, 2))
}
