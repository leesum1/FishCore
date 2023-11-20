package leesum
import chisel3._
import chisel3.util._
import leesum.moniter.CommitMonitorPort
import spire.math

class RedirectPC extends Bundle {
  val target = UInt(64.W)
}

class CommitStage(num_commit_port: Int, monitor_en: Boolean = false)
    extends Module {
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
    val amo_commit = Decoupled(Bool())
    // csr
    val csr_commit = Decoupled(Bool())
    // branch
    val branch_commit = Decoupled(new RedirectPC())

    // csr regfile direct access
    val direct_read_ports = Input(new CSRDirectReadPorts)
    val direct_write_ports = Output(new CSRDirectWritePorts)

    // privilege mode
    val cur_privilege_mode = Output(UInt(2.W))

    val commit_monitor = if (monitor_en) {
      Some(Output(Vec(num_commit_port, Valid(new CommitMonitorPort))))
    } else {
      None
    }
  })

  val rob_valid_seq = io.rob_commit_ports.map(_.valid)
  val rob_exception_seq = io.rob_commit_ports.map(_.bits.exception.valid)
  val rob_data_seq = io.rob_commit_ports.map(_.bits)
  val pop_ack = WireInit(VecInit(Seq.fill(num_commit_port)(false.B)))

  val flush_next = RegInit(false.B)
  val privilege_mode = RegInit(3.U(2.W)) // machine mode

  io.cur_privilege_mode := privilege_mode

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
    val alu_mul_div_op =
      FuOP.is_alu(entry.fu_op) || FuOP.is_mul(entry.fu_op) || FuOP.is_div_rem(
        entry.fu_op
      )

    assert(alu_mul_div_op, "fu_op must be alu, mul or div")
    assert(entry.exception.valid === false.B)
    when(alu_mul_div_op) {
      gpr_commit_port.write(entry.rd_addr, entry.result)
      ack := true.B
    }
  }

  def retire_load(
      entry: ScoreBoardEntry,
      gpr_commit_port: GPRsWritePort,
      ack: Bool
  ): Unit = {
    assert(FuOP.is_lsu(entry.fu_op), "fu_op must be lsu")
    assert(entry.exception.valid === false.B)
    when(FuOP.is_load(entry.fu_op)) {
      assert(entry.complete === true.B, "load must be complete")
      gpr_commit_port.write(entry.rd_addr, entry.result)
      ack := true.B
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
      store_commit.bits := true.B
      when(store_commit.fire) {
        ack := true.B
      }
    }
  }

  def retire_branch(
      entry: ScoreBoardEntry,
      gpr_commit_port: GPRsWritePort,
      ack: Bool
  ): Unit = {
    assert(
      FuOP.is_branch(entry.fu_op) || FuOP.is_jalr(entry.fu_op) || FuOP.is_jal(
        entry.fu_op
      ),
      "fu_op must be branch"
    )
    assert(entry.exception.valid === false.B)
    val mis_predict = entry.bp.is_miss_predict
    when(mis_predict) {
      io.branch_commit.valid := true.B
//      when(io.branch_commit.fire) {
      io.branch_commit.bits.target := entry.bp.predict_pc
      flush_next := true.B
      ack := true.B
      when(FuOP.is_jal(entry.fu_op) || FuOP.is_jalr(entry.fu_op)) {
        gpr_commit_port.write(entry.rd_addr, entry.result)
      }
//      }
    }.otherwise {
      when(FuOP.is_jal(entry.fu_op) || FuOP.is_jalr(entry.fu_op)) {
        gpr_commit_port.write(entry.rd_addr, entry.result)
      }
      ack := true.B
    }
  }

  def retire_none(entry: ScoreBoardEntry, ack: Bool): Unit = {
    assert(entry.exception.valid === false.B)

    // TODO: unimplemented
    when(entry.fu_op === FuOP.SFenceVMA) {
      val mstatus = new MstatusFiled(io.direct_read_ports.mstatus)
      val require_privi =
        Mux(mstatus.tvm, Privilegelevel.M.U, Privilegelevel.S.U)
      printf("SFenceVMA at %x\n", entry.pc)

      when(privilege_mode >= require_privi) {
        flush_next := true.B
        io.branch_commit.valid := true.B
        // same as interrupt to reduce area
        io.branch_commit.bits.target := entry.pc + Mux(
          entry.is_rv32,
          2.U,
          4.U
        )
      }.otherwise {
        val vma_entry = WireInit(entry)
        vma_entry.exception.valid := true.B
        vma_entry.exception.cause := ExceptionCause.illegal_instruction
        vma_entry.exception.tval := vma_entry.inst // TODO: set tval 0

        retire_exception(vma_entry, ack)
      }

    }.elsewhen(entry.fu_op === FuOP.Fence) {
      printf("Fence at %x\n", entry.pc)
    }.elsewhen(entry.fu_op === FuOP.FenceI) {
      printf("FenceI at %x\n", entry.pc)
    }.elsewhen(entry.fu_op === FuOP.WFI) {
      printf("WFI at %x\n", entry.pc)
    }

    ack := true.B
  }

  // TODO: exception
  def retire_exception(entry: ScoreBoardEntry, ack: Bool): Unit = {
    assert(entry.exception.valid === true.B)

    val mstatus = new MstatusFiled(io.direct_read_ports.mstatus)
    val medeleg = io.direct_read_ports.medeleg
    val exception_mcause = Mux(
      entry.fu_op === FuOP.Ecall,
      ExceptionCause.get_call_cause(privilege_mode),
      entry.exception.cause
    ).asUInt

    val trap_to_smode = medeleg(
      exception_mcause
    ) && privilege_mode <= Privilegelevel.S.U

    when(trap_to_smode) {
      // s mode
      val stvec = new MtvecFiled(io.direct_read_ports.stvec)
      io.branch_commit.valid := true.B
      io.branch_commit.bits.target := stvec.get_exception_pc

//      when(io.branch_commit.fire) {
      ack := true.B
      flush_next := true.B
      privilege_mode := Privilegelevel.S.U

      io.direct_write_ports.mstatus.valid := true.B
      io.direct_write_ports.mstatus.bits := mstatus
        .get_smode_exception_mstatus(
          privilege_mode
        )
      io.direct_write_ports.sepc.valid := true.B
      io.direct_write_ports.sepc.bits := entry.pc
      io.direct_write_ports.scause.valid := true.B
      io.direct_write_ports.scause.bits := exception_mcause
      io.direct_write_ports.stval.valid := true.B
      io.direct_write_ports.stval.bits := entry.exception.tval

//    }

    }.otherwise {
      // m mode
      val mtvec = new MtvecFiled(io.direct_read_ports.mtvec)
      io.branch_commit.valid := true.B
      io.branch_commit.bits.target := mtvec.get_exception_pc

//    when(io.branch_commit.fire) {
      ack := true.B
      flush_next := true.B
      privilege_mode := Privilegelevel.M.U

      io.direct_write_ports.mepc.valid := true.B
      io.direct_write_ports.mepc.bits := entry.pc
      io.direct_write_ports.mcause.valid := true.B
      io.direct_write_ports.mcause.bits := exception_mcause
      io.direct_write_ports.mtval.valid := true.B
      io.direct_write_ports.mtval.bits := entry.exception.tval
      io.direct_write_ports.mstatus.valid := true.B
      io.direct_write_ports.mstatus.bits := mstatus
        .get_mmode_exception_mstatus(
          privilege_mode
        )
//      }
    }

    // ------------------
    // assert
    // ------------------

    assume(
      entry.complete,
      "exception must be complete"
    )
  }

  def retire_mret(entry: ScoreBoardEntry, ack: Bool): Unit = {
    assert(entry.fu_op === FuOP.Mret, "fu_op must be mret")
    assert(entry.exception.valid === false.B)
    assert(entry.complete === true.B, "mret must be complete")

    val mstatus = new MstatusFiled(io.direct_read_ports.mstatus)
    val mepc = io.direct_read_ports.mepc

    // supposing xPP holds the value y
    val y = mstatus.mpp
    // the privilege mode is changed to xPP
    privilege_mode := y

    // (If y!=M, x RET also sets MPRV=0.)
    // reference to  https://github.com/riscv/riscv-isa-manual/pull/929
    val new_mstatus = mstatus.get_mret_mstatus(y =/= Privilegelevel.M.U(2.W))

    io.branch_commit.valid := true.B
//    when(io.branch_commit.fire) {
    io.branch_commit.bits.target := mepc
    io.direct_write_ports.mstatus.valid := true.B
    io.direct_write_ports.mstatus.bits := new_mstatus
    flush_next := true.B
    ack := true.B
//    }
  }

  def retire_sret(entry: ScoreBoardEntry, ack: Bool): Unit = {
    assert(entry.fu_op === FuOP.Sret, "fu_op must be sret")
    assert(entry.exception.valid === false.B)
    assert(entry.complete === true.B, "sret must be complete")

    val sstatus = new MstatusFiled(io.direct_read_ports.mstatus)
    val sepc = io.direct_read_ports.sepc

    // SRET should also raise an illegal instruction exception when TSR=1 in mstatus
    when(sstatus.tsr) {
      val sret_entry = WireInit(entry)
      sret_entry.exception.valid := true.B
      sret_entry.exception.cause := ExceptionCause.illegal_instruction
      sret_entry.exception.tval := entry.inst // TODO: set tval 0
      retire_exception(sret_entry, ack)
    }.otherwise {
      // supposing xPP holds the value y, 0: user mode 1: s mode
      val y = Mux(sstatus.spp, Privilegelevel.S.U(2.W), Privilegelevel.U.U(2.W))
      // the privilege mode is changed to xPP
      privilege_mode := y

      // (If y!=M, x RET also sets MPRV=0.)
      // reference to  https://github.com/riscv/riscv-isa-manual/pull/929
      val new_mstatus = sstatus.get_sret_mstatus(y =/= Privilegelevel.M.U(2.W))

      io.branch_commit.valid := true.B
      io.branch_commit.bits.target := sepc

      io.direct_write_ports.mstatus.valid := true.B
      io.direct_write_ports.mstatus.bits := new_mstatus
      ack := true.B
      flush_next := true.B

    }

  }

  // TODO: implement not correct!!!!!!!!!
  def retire_csr(
      entry: ScoreBoardEntry,
      gpr_commit_port: GPRsWritePort,
      csr_commit_port: DecoupledIO[Bool],
      ack: Bool
  ): Unit = {
    assert(FuOP.is_csr(entry.fu_op), "fu_op must be csr")
    assert(entry.exception.valid === false.B)

    val sIdle :: sACK :: Nil = Enum(2)
    val state = RegInit(sIdle)
    state.suggestName("csr_state")

    switch(state) {
      is(sIdle) {
        assert(entry.complete === false.B, "csr must be not complete")
        csr_commit_port.bits := true.B
        csr_commit_port.valid := true.B
        when(csr_commit_port.fire) {
          state := sACK
        }
      }
      is(sACK) {
        assert(entry.complete === true.B, "csr must be complete")
        gpr_commit_port.write(entry.rd_addr, entry.result)
        ack := true.B
        state := sIdle
      }
    }
  }

  def retire_amo(
      entry: ScoreBoardEntry,
      gpr_commit_port: GPRsWritePort,
      amo_commit_port: DecoupledIO[Bool],
      ack: Bool
  ): Unit = {
    assert(entry.exception.valid === false.B)
    assert(FuOP.is_lsu(entry.fu_op), "fu_op must be lsu")
    when(FuOP.is_atomic(entry.fu_op)) {
      assert(entry.complete === true.B, "load must be complete")
      gpr_commit_port.write(entry.rd_addr, entry.result)
      ack := true.B
    }
  }

  io.mmio_commit.noenq()
  io.store_commit.noenq()
  io.amo_commit.noenq()
  io.csr_commit.noenq()
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

  io.direct_write_ports.clear()

  // -----------------------
  // retire logic
  // -----------------------

  // first inst
  when(rob_valid_seq.head && rob_data_seq.head.complete && !flush_next) {
    when(rob_data_seq.head.exception.valid) {
      retire_exception(rob_data_seq.head, pop_ack.head)
    }.elsewhen(
      FuOP.is_xret(rob_data_seq.head.fu_op)
    ) {
      when(rob_data_seq.head.fu_op === FuOP.Mret) {
        retire_mret(rob_data_seq.head, pop_ack.head)
      }.otherwise {
        retire_sret(rob_data_seq.head, pop_ack.head)
      }
    }.otherwise {
      switch(rob_data_seq.head.fu_type) {
        is(FuType.Mul, FuType.Alu, FuType.Div) {
          retire_alu(
            rob_data_seq.head,
            io.gpr_commit_ports.head,
            pop_ack.head
          )
        }
        is(FuType.Lsu) {
          when(FuOP.is_load(rob_data_seq.head.fu_op)) {
            retire_load(
              rob_data_seq.head,
              io.gpr_commit_ports.head,
              pop_ack.head
            )
          }.elsewhen(FuOP.is_store(rob_data_seq.head.fu_op)) {
            retire_store(
              rob_data_seq.head,
              io.store_commit,
              pop_ack.head
            )
          }.elsewhen(FuOP.is_atomic(rob_data_seq.head.fu_op)) {
            retire_amo(
              rob_data_seq.head,
              io.gpr_commit_ports.head,
              io.amo_commit,
              pop_ack.head
            )
          }.otherwise {
            assert(false.B, "lsu op error")
          }
        }
        is(FuType.Br) {
          retire_branch(
            rob_data_seq.head,
            io.gpr_commit_ports.head,
            pop_ack.head
          )
        }
        is(FuType.Csr) {
          retire_csr(
            rob_data_seq.head,
            io.gpr_commit_ports.head,
            io.csr_commit,
            pop_ack.head
          )
        }
        is(FuType.None) {
          retire_none(rob_data_seq.head, pop_ack.head)
        }
      }
    }
  }.elsewhen(
    rob_valid_seq.head && !rob_data_seq.head.complete && !flush_next
  ) {
    // for csr, mmio, amo the complete flag is not set
    assert(
      rob_data_seq.head.exception.valid === false.B,
      "rob entry must be not exception"
    )
    switch(rob_data_seq.head.fu_type) {
      is(FuType.Lsu) {
        when(
          rob_data_seq.head.lsu_io_space && FuOP.is_load(
            rob_data_seq.head.fu_op
          )
        ) {
          io.mmio_commit.valid := true.B
          io.mmio_commit.bits := true.B
        }.elsewhen(FuOP.is_atomic(rob_data_seq.head.fu_op)) {
          io.amo_commit.valid := true.B
          io.amo_commit.bits := true.B
        }
      }
      is(FuType.Csr) {
        io.csr_commit.valid := true.B
        io.csr_commit.bits := true.B
      }
    }
  }

  // ----------------------
  // interrupt
  // ----------------------

  val interrupt_inject_types = VecInit(
    Seq(
      FuType.Alu.asUInt,
      FuType.Mul.asUInt,
      FuType.Div.asUInt,
      FuType.Lsu.asUInt,
      FuType.Br.asUInt,
      FuType.Csr.asUInt
    )
  )

  val has_interrupt = WireInit(false.B)

  dontTouch(has_interrupt)

  when(
    pop_ack.head && !rob_data_seq.head.exception.valid && interrupt_inject_types
      .contains(
        rob_data_seq.head.fu_type.asUInt
      )
  ) {
    val mip_and_mie = io.direct_read_ports.mip & io.direct_read_ports.mie
    dontTouch(mip_and_mie)

    val mstatus = new MstatusFiled(io.direct_read_ports.mstatus)
    val mideleg = io.direct_read_ports.mideleg

    val interrupt_mmode =
      mstatus.mie && privilege_mode === Privilegelevel.M.U || privilege_mode < Privilegelevel.M.U
    val mmode_pending = new MipFiled(mip_and_mie & (~mideleg).asUInt)
    val mmode_has_interrupt = mmode_pending.any_interrupt

    dontTouch(interrupt_mmode)
    dontTouch(mmode_has_interrupt)

    val interrupt_smode =
      mstatus.sie && privilege_mode === Privilegelevel.S.U || privilege_mode < Privilegelevel.S.U
    val smode_pending = new MipFiled(mip_and_mie & mideleg)
    val smode_has_interrupt = smode_pending.any_interrupt

    dontTouch(interrupt_smode)
    dontTouch(smode_has_interrupt)

    val normal_pc = rob_data_seq.head.pc + Mux(
      rob_data_seq.head.is_rv32,
      2.U,
      4.U
    )

    val br_pc = rob_data_seq.head.bp.predict_pc

    val new_pc = Mux(
      rob_data_seq.head.bp.is_miss_predict,
      br_pc,
      normal_pc
    )

    when(interrupt_mmode && mmode_has_interrupt) {
      has_interrupt := true.B

      val cause = mmode_pending.get_priority_interupt
      val mtvec = new MtvecFiled(io.direct_read_ports.mtvec)
      flush_next := true.B
      privilege_mode := Privilegelevel.M.U

      io.branch_commit.valid := true.B
      io.branch_commit.bits.target := mtvec.get_interrupt_pc(cause.asUInt)

      io.direct_write_ports.mepc.valid := true.B
      io.direct_write_ports.mepc.bits := new_pc

      io.direct_write_ports.mcause.valid := true.B

      val cause_low = Wire(UInt(63.W))
      cause_low := cause.asUInt

      io.direct_write_ports.mcause.bits := Cat(true.B, cause_low)

      io.direct_write_ports.mstatus.valid := true.B
      io.direct_write_ports.mstatus.bits := mstatus.get_mmode_exception_mstatus(
        privilege_mode
      )
    }.elsewhen(interrupt_smode && smode_has_interrupt) {
      has_interrupt := true.B

      val cause = smode_pending.get_priority_interupt
      val stvec = new MtvecFiled(io.direct_read_ports.stvec)

      flush_next := true.B
      privilege_mode := Privilegelevel.S.U

      io.branch_commit.valid := true.B
      io.branch_commit.bits.target := stvec.get_interrupt_pc(cause.asUInt)

      io.direct_write_ports.sepc.valid := true.B
      io.direct_write_ports.sepc.bits := new_pc

      val cause_low = Wire(UInt(63.W))
      cause_low := cause.asUInt
      io.direct_write_ports.scause.valid := true.B
      io.direct_write_ports.scause.bits := Cat(true.B, cause_low)

      io.direct_write_ports.mstatus.valid := true.B
      io.direct_write_ports.mstatus.bits := mstatus.get_smode_exception_mstatus(
        privilege_mode
      )
    }
  }

  // ---------------------
  // second inst
  // ---------------------
  when(
    rob_valid_seq(1) && rob_data_seq(
      1
    ).complete && !flush_next && !has_interrupt
  ) {

    // TODO: more constraint on the second inst?
    when(
      pop_ack.head && !rob_data_seq.head.exception.valid
        && rob_data_seq.head.fu_type =/= FuType.Br && rob_data_seq.head.fu_type =/= FuType.None
    ) {
      val retire_fu_type_seq = VecInit(
        Seq(
          FuType.Alu.asUInt,
          FuType.Mul.asUInt,
          FuType.Div.asUInt
        )
      )
      when(retire_fu_type_seq.contains(rob_data_seq(1).fu_type.asUInt)) {
        retire_alu(rob_data_seq(1), io.gpr_commit_ports(1), pop_ack(1))
      }
    }
  }

  // -----------------------
  // commit monitor
  // -----------------------

  if (monitor_en) {
    val commitMonitorSignals = io.commit_monitor.get

    for (idx <- 0 until num_commit_port) {
      commitMonitorSignals(idx).valid := io.rob_commit_ports(idx).fire
      commitMonitorSignals(idx).bits.pc := rob_data_seq(idx).pc
      commitMonitorSignals(idx).bits.inst := rob_data_seq(idx).inst
      commitMonitorSignals(idx).bits.fu_type := rob_data_seq(idx).fu_type
      commitMonitorSignals(idx).bits.fu_op := rob_data_seq(idx).fu_op
      commitMonitorSignals(idx).bits.exception := rob_data_seq(idx).exception
      commitMonitorSignals(idx).bits.is_mmio := rob_data_seq(idx).lsu_io_space

      // override exception cause
      commitMonitorSignals(idx).bits.exception.cause := Mux(
        rob_data_seq(idx).fu_op === FuOP.Ecall,
        ExceptionCause.get_call_cause(privilege_mode),
        rob_data_seq(idx).exception.cause
      )
    }
  }

  // -----------------------
  // assert
  // -----------------------

  assert(
    CheckOrder(VecInit(rob_valid_seq)),
    "rob_commit_ports must be ordered"
  )

  when(
    rob_valid_seq.head && rob_data_seq.head.complete
  ) {
    assert(
      rob_data_seq.head.pc =/= 0.U,
      "pc must not be zero"
    )
  }

  for (i <- 0 until num_commit_port) {
    when(rob_valid_seq(i) && rob_data_seq(i).complete) {
      assume(
        rob_data_seq(i).pc =/= 0.U,
        "pc must not be zero"
      )

      when(rob_data_seq(i).bp.is_miss_predict) {
        assume(
          rob_data_seq(i).bp.predict_pc =/= 0.U,
          "predict_pc must not be zero"
        )
        assume(rob_data_seq(i).fu_type === FuType.Br, "fu_type must be branch")
      }
    }
  }

}

object gen_commit_stage_verilog extends App {
  GenVerilogHelper(
    new CommitStage(num_commit_port = 2, monitor_en = true)
  )
}
