package leesum

import chisel3._
import chisel3.util._

class OperandByPassReq extends Bundle {
  val rs_addr = UInt(5.W)
}

class OperandByPassResp extends Bundle {
  val rs_data = UInt(64.W)
  val fwd_valid = Bool()
  val fwd_stall = Bool()
}

class ReOrderBuffer(
    val entries: Int,
    val num_push_ports: Int,
    val num_pop_ports: Int
) extends Module {
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

      val operand_bypass_req =
        Input(Vec(num_push_ports * 2, new OperandByPassReq()))
      val operand_bypass_resp = Output(
        Vec(num_push_ports * 2, new OperandByPassResp())
      )

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
    "write-back conflict"
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
        rob.random_access(rob_idx).valid,
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

  // ------------------------------------------
  // register rename logic, renaming in rob
  // ------------------------------------------

  // register rename table is used to rename rd_addr
  // the content of rename_table is the index of rob entry
  val rename_table = new RenameTable(
    set_ports_num = num_push_ports,
    clear_ports_num = num_pop_ports,
    rob_nums = entries
  )

  val rename_set_ports = Wire(
    Vec(num_push_ports, Valid(new RenameSetPort(entries)))
  )
  val rename_clear_ports = Wire(
    Vec(num_pop_ports, Valid(new RenameClearPort(entries)))
  )

  for (i <- 0 until num_push_ports) {
    val rd_addr = io.push_ports(i).bits.rd_addr
    rename_set_ports(i).valid := io.push_ports(i).fire && (rd_addr =/= 0.U)
    rename_set_ports(i).bits.rd_addr := rd_addr
    rename_set_ports(i).bits.rob_ptr := rob.push_ptr_seq(i)
  }
  for (i <- 0 until num_pop_ports) {
    val rd_addr = io.pop_ports(i).bits.rd_addr
    rename_clear_ports(i).valid := io.pop_ports(i).fire && (rd_addr =/= 0.U)
    rename_clear_ports(i).bits.rd_addr := rd_addr
    rename_clear_ports(i).bits.rob_ptr := rob.pop_ptr_seq(i)
  }

  rename_table.set_clear_flush(
    set_ports = rename_set_ports,
    clear_ports = rename_clear_ports,
    flush_port = io.flush
  )

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

  def check_operand_bypass(
      bypass_req: OperandByPassReq,
      bypass_resp: OperandByPassResp
  ) = {
    val rob_idx_valid = rename_table.rand_access(bypass_req.rs_addr).valid
    val rob_idx = rename_table.rand_access(bypass_req.rs_addr).bits

    when(rob_idx_valid) {
      val fu_id_eq =
        VecInit(
          fu_id.zip(fu_valid).zip(fu_result_valid).map {
            case ((id, valid), result_valid) =>
              id === rob_idx & valid & result_valid
          }
        )

      assert(
        PopCount(fu_id_eq) <= 1.U,
        "rs1 bypass error, fu write back conflict"
      )
      when(fu_id_eq.reduce(_ | _)) {
        // fu write back with higher priority
        bypass_resp.fwd_valid := true.B
        bypass_resp.fwd_stall := false.B
        bypass_resp.rs_data := fu_result(fu_id_eq.indexWhere(_ === true.B))
      }.otherwise {
        val rob_entry = rob.content(rob_idx)
        assert(
          rob_entry.valid,
          "rs1 bypass error, rob entry must be valid"
        )

        when(rob_entry.bits.rd_data_valid()) {
          bypass_resp.fwd_valid := true.B
          bypass_resp.fwd_stall := false.B
          bypass_resp.rs_data := rob_entry.bits.result
        }.otherwise {
          bypass_resp.fwd_valid := false.B
          bypass_resp.fwd_stall := true.B
        }
      }
    }
  }

  io.operand_bypass_resp.foreach(bypass_resp => {
    bypass_resp.fwd_valid := false.B
    bypass_resp.fwd_stall := false.B
    bypass_resp.rs_data := DontCare
  })

  require(io.operand_bypass_resp.length == io.operand_bypass_req.length)

  for (i <- 0 until io.operand_bypass_req.length) {
    check_operand_bypass(io.operand_bypass_req(i), io.operand_bypass_resp(i))
  }

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
}

object gen_ReOrderBuffer_verilog extends App {
  GenVerilogHelper(new ReOrderBuffer(8, 2, 2))
}