package leesum
import chisel3._
import chisel3.util.{
  Decoupled,
  DecoupledIO,
  MuxLookup,
  PopCount,
  Valid,
  is,
  isPow2,
  log2Ceil,
  switch
}

class RedirectPC extends Bundle {
  val target = UInt(64.W)
}

class CommitStage(num_commit_port: Int) extends Module {
  val io = IO(new Bundle {

    val rob_commit_ports =
      Vec(num_commit_port, Flipped(Decoupled(new ScoreBoardEntry)))
    val flush = Output(Bool())

    // gpr
    val gpr_commit_ports =
      Vec(num_commit_port, Flipped(new GPRsWritePort))
    // lsu
    val mmio_commit = Decoupled(Bool())
    val store_commit = Decoupled(Bool())
    // branch
    val branch_commit = Decoupled(new RedirectPC())
  })

  val rob_valid_seq = io.rob_commit_ports.map(_.valid)
  val rob_exception_seq = io.rob_commit_ports.map(_.bits.exception.valid)
  val rob_data_seq = io.rob_commit_ports.map(_.bits)
  val pop_ack = WireInit(VecInit(Seq.fill(num_commit_port)(false.B)))

  val flush_next = RegInit(false.B)

  when(flush_next) {
    flush_next := false.B
  }

  io.flush := flush_next

  assert(
    CheckOrder(pop_ack),
    "rob_commit_ports must be ordered"
  )

  def retire_alu(
      entry: ScoreBoardEntry,
      gpr_commit_port: GPRsWritePort,
      ack: Bool
  ): Unit = {
    assert(FuOP.is_alu(entry.fu_op), "fu_op must be lsu")
    assert(entry.exception.valid === false.B)
    when(FuOP.is_alu(entry.fu_op)) {
      gpr_commit_port.write(entry.rd_addr, entry.result)
      ack := true.B
    }
  }

  def retire_load(
      entry: ScoreBoardEntry,
      gpr_commit_port: GPRsWritePort,
      mmio_commit: DecoupledIO[Bool],
      ack: Bool
  ): Unit = {
    assert(FuOP.is_lsu(entry.fu_op), "fu_op must be lsu")
    assert(entry.exception.valid === false.B)
    when(FuOP.is_load(entry.fu_op)) {
      when(entry.lsu_io_space && !entry.complete) {

        mmio_commit.valid := true.B
        when(io.mmio_commit.ready) {
          mmio_commit.bits := true.B
          ack := true.B
        }
      }.elsewhen(entry.complete) {
        gpr_commit_port.write(entry.rd_addr, entry.result)
        ack := true.B
      }
    }
  }

  def retire_store(
      entry: ScoreBoardEntry,
      store_commit: DecoupledIO[Bool],
      ack: Bool
  ): Unit = {
    assert(FuOP.is_lsu(entry.fu_op), "fu_op must be lsu")
    assert(entry.exception.valid === false.B)
    assert(entry.complete === true.B, "store must be complete")
    when(FuOP.is_store(entry.fu_op)) {
      // valid rely on store_commit.ready
      store_commit.valid := true.B
      when(store_commit.fire) {
        store_commit.bits := true.B
        ack := true.B
      }
    }
  }

  def retire_branch(
      entry: ScoreBoardEntry,
      gpr_commit_port: GPRsWritePort,
      ack: Bool
  ): Unit = {
    assert(FuOP.is_branch(entry.fu_op), "fu_op must be branch")
    assert(entry.exception.valid === false.B)
    val mis_predict = entry.bp.is_miss_predict
    when(mis_predict) {
      io.branch_commit.valid := true.B
      when(io.branch_commit.fire) {
        io.branch_commit.bits.target := entry.bp.predict_pc
        flush_next := true.B
        ack := true.B
        when(FuOP.is_jal(entry.fu_op) || FuOP.is_jalr(entry.fu_op)) {
          gpr_commit_port.write(entry.rd_addr, entry.result)
        }
      }
    }.otherwise {
      when(FuOP.is_jal(entry.fu_op) || FuOP.is_jalr(entry.fu_op)) {
        gpr_commit_port.write(entry.rd_addr, entry.result)
      }
      ack := true.B
    }
  }

  io.mmio_commit.noenq()
  io.store_commit.noenq()
  io.gpr_commit_ports.foreach(gpr => {
    gpr.addr := 0.U
    gpr.wdata := 0.U
    gpr.wen := false.B
  })
  io.branch_commit.noenq()
  io.rob_commit_ports.foreach(_.nodeq())

  io.rob_commit_ports.zip(pop_ack).foreach { case (port, ack) =>
    port.ready := ack
  }

  when(rob_valid_seq.head && rob_data_seq.head.complete && !flush_next) {
    when(rob_data_seq.head.exception.valid) {
      assert(
        rob_data_seq.head.complete === true.B,
        "rob entry must be complete"
      )
    }.otherwise {
      switch(rob_data_seq.head.fu_type) {
        is(FuType.Alu) {
          retire_alu(
            rob_data_seq.head,
            io.gpr_commit_ports.head,
            pop_ack.head
          )
        }
        is(FuType.Lsu) {
          retire_load(
            rob_data_seq.head,
            io.gpr_commit_ports.head,
            io.mmio_commit,
            pop_ack.head
          )
          retire_store(
            rob_data_seq.head,
            io.store_commit,
            pop_ack.head
          )
        }
        is(FuType.Br) {
          retire_branch(
            rob_data_seq.head,
            io.gpr_commit_ports.head,
            pop_ack.head
          )
        }
      }

    }
  }

  assert(
    CheckOrder(VecInit(rob_valid_seq)),
    "rob_commit_ports must be ordered"
  )
}

object gen_commit_stage_verilog extends App {
  GenVerilogHelper(
    new CommitStage(num_commit_port = 2)
  )
}
