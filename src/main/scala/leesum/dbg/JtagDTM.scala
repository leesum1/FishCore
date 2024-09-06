package leesum.dbg
import chisel3._
import chisel3.util.{DecoupledIO, _}
import leesum.{CSRBitField, CSRMap, GenVerilogHelper}

class JtagIO(as_master: Boolean) extends Bundle {
  val tck = if (as_master) Output(Bool()) else Input(Bool())
  val rst = if (as_master) Output(Bool()) else Input(Bool())
  val tms = if (as_master) Output(Bool()) else Input(Bool())
  val tdi = if (as_master) Output(Bool()) else Input(Bool())
  val tdi_en = if (as_master) Output(Bool()) else Input(Bool())
  val tdo = if (as_master) Input(Bool()) else Output(Bool())
  val tdo_en = if (as_master) Input(Bool()) else Output(Bool())

  def clear(): Unit = {
    if (as_master) {
      tck := false.B
      rst := false.B
      tms := false.B
      tdi := false.B
      tdi_en := false.B
    } else {
      tdo := false.B
      tdo_en := false.B
    }
  }
}

object JtagState extends ChiselEnum {
  val TestLogicReset = Value(0.U)
  val RunTestIdle = Value(1.U)
  val SelectDrScan = Value(2.U)
  val CaptureDr = Value(3.U)
  val ShiftDr = Value(4.U)
  val Exit1Dr = Value(5.U)
  val PauseDr = Value(6.U)
  val Exit2Dr = Value(7.U)
  val UpdateDr = Value(8.U)
  val SelectIrScan = Value(9.U)
  val CaptureIr = Value(10.U)
  val ShiftIr = Value(11.U)
  val Exit1Ir = Value(12.U)
  val PauseIr = Value(13.U)
  val Exit2Ir = Value(14.U)
  val UpdateIr = Value(15.U)

  def is_update_dr(): Bool = {
    this.Value === UpdateDr
  }

  def get_next_state(cur: JtagState.Type, tms_i: Bool): JtagState.Type = {
    val next_state = Wire(JtagState())
    next_state := cur

    switch(cur) {
      is(TestLogicReset) {
        when(tms_i) {
          next_state := TestLogicReset
        }.otherwise {
          next_state := RunTestIdle
        }
      }
      is(RunTestIdle) {
        when(tms_i) {
          next_state := SelectDrScan
        }.otherwise {
          next_state := RunTestIdle
        }
      }
      is(SelectDrScan) {
        when(tms_i) {
          next_state := SelectIrScan
        }.otherwise {
          next_state := CaptureDr
        }
      }
      is(CaptureDr) {
        when(tms_i) {
          next_state := Exit1Dr
        }.otherwise {
          next_state := ShiftDr
        }
      }
      is(ShiftDr) {
        when(tms_i) {
          next_state := Exit1Dr
        }.otherwise {
          next_state := ShiftDr
        }
      }
      is(Exit1Dr) {
        when(tms_i) {
          next_state := UpdateDr
        }.otherwise {
          next_state := PauseDr
        }
      }
      is(PauseDr) {
        when(tms_i) {
          next_state := Exit2Dr
        }.otherwise {
          next_state := PauseDr
        }
      }
      is(Exit2Dr) {
        when(tms_i) {
          next_state := UpdateDr
        }.otherwise {
          next_state := ShiftDr
        }
      }
      is(UpdateDr) {
        when(tms_i) {
          next_state := SelectDrScan
        }.otherwise {
          next_state := RunTestIdle
        }
      }
      is(SelectIrScan) {
        when(tms_i) {
          next_state := TestLogicReset
        }.otherwise {
          next_state := CaptureIr
        }
      }
      is(CaptureIr) {
        when(tms_i) {
          next_state := Exit1Ir
        }.otherwise {
          next_state := ShiftIr
        }
      }
      is(ShiftIr) {
        when(tms_i) {
          next_state := Exit1Ir
        }.otherwise {
          next_state := ShiftIr
        }
      }
      is(Exit1Ir) {
        when(tms_i) {
          next_state := UpdateIr
        }.otherwise {
          next_state := PauseIr
        }
      }
      is(PauseIr) {
        when(tms_i) {
          next_state := Exit2Ir
        }.otherwise {
          next_state := PauseIr
        }
      }
      is(Exit2Ir) {
        when(tms_i) {
          next_state := UpdateIr
        }.otherwise {
          next_state := ShiftIr
        }
      }
      is(UpdateIr) {
        when(tms_i) {
          next_state := SelectDrScan
        }.otherwise {
          next_state := RunTestIdle
        }
      }
    }
    next_state
  }
}

class JtagDTM(dm_config: DebugModuleConfig) extends Module {
  val io = IO(new Bundle {
    val jtag = new JtagIO(as_master = false)
    val dmi_req = DecoupledIO(new DMIReq(dm_config.abits))
    val dmi_resp = Flipped(DecoupledIO(new DMIResp(dm_config.abits)))

    val dmi_reset_valid = Output(Bool())
    val dmi_hard_reset_valid = Output(Bool())
    val jtag_state = Output(JtagState())
    val jtag_ir = Output(UInt(dm_config.ir_length.W))
  })

  io.jtag.clear()
  io.dmi_req.noenq()
  io.dmi_resp.nodeq()
  io.dmi_reset_valid := false.B
  io.dmi_hard_reset_valid := false.B

  val dtm_reg_max_width = 34 + dm_config.abits

  // JTAG state machine
  val jtag_state = RegInit(JtagState.TestLogicReset)
  jtag_state := JtagState.get_next_state(jtag_state, io.jtag.tms)
  io.jtag_state := jtag_state

  val jtag_tap_reset = jtag_state === JtagState.TestLogicReset || reset.asBool
  dontTouch(jtag_tap_reset)
  val ir_reg = RegInit(DbgPKG.JtagDTM_IDCODE.U(dm_config.ir_length.W))
  io.jtag_ir := ir_reg
  // shift register
  val ir_shift_reg = Module(new JtagShiftReg(dm_config.ir_length))
  val dr_shift_reg = Module(new JtagShiftReg(dtm_reg_max_width))

  val clock_falling = WireInit((!clock.asUInt).asClock)
  dontTouch(clock_falling)

  ir_shift_reg.io.clear()
  dr_shift_reg.io.clear()

  // --------------------
  // JtagDTM register
  // --------------------

  val dtm_dtmcs_init_value = new CSRBitField(0)
  dtm_dtmcs_init_value.set_field(DTMCSMask.abits, dm_config.abits)
  dtm_dtmcs_init_value.set_field(DTMCSMask.version, 1) // 0.13 and 1.0
  dtm_dtmcs_init_value.set_field(DTMCSMask.idle, 5)

  val dtm_idcode = RegInit("xdeadbeef".U(dtm_reg_max_width.W))

  println(
    "dtm_dtmcs_init_value.getRawValue: " + dtm_dtmcs_init_value.get_raw
  )
  val dtm_dtmcs = withReset(jtag_tap_reset) {
    RegInit(
      dtm_dtmcs_init_value.get_raw.U(dtm_reg_max_width.W)
    )
  }

  dontTouch(dtm_dtmcs)

  val dtm_dmi = withReset(jtag_tap_reset) { RegInit(0.U(dtm_reg_max_width.W)) }
  val dtm_bypass0 = withReset(jtag_tap_reset) {
    RegInit(0.U(dtm_reg_max_width.W))
  }
  val dtm_bypass1 = withReset(jtag_tap_reset) {
    RegInit(0.U(dtm_reg_max_width.W))
  }
  val dmt_custom = withReset(jtag_tap_reset) {
    RegInit(0.U(dtm_reg_max_width.W))
  }

  val dtm_csrs = new CSRMap()
  val dtm_dmi_filed = new DTMDMIFiled(dtm_dmi)
  // -------------------------
  // Flags
  // -------------------------
  val dmi_busy = withReset(jtag_tap_reset) { RegInit(false.B) }
  val dmi_req_op_is_read = withReset(jtag_tap_reset) { RegInit(false.B) }
  val dmi_sticky_error = withReset(jtag_tap_reset) { RegInit(false.B) }
  val can_dmi_send_req = withReset(jtag_tap_reset) { RegInit(false.B) }

  val in_dmi_error = dmi_sticky_error || dmi_busy

  // -------------------------
  // map normal read write func define
  // -------------------------

  val dtm_dtmcs_write_func = (addr: UInt, reg: UInt, wdata: UInt) => {
    val new_dtm_dtmcs_filed = new DTMDTMCSFiled(wdata(31, 0))

    when(new_dtm_dtmcs_filed.dmireset) {
      io.dmi_reset_valid := true.B
      dmi_sticky_error := false.B
      printf("dtmcs dmi reset\n")
    }

    when(new_dtm_dtmcs_filed.dmihardreset) {
      io.dmi_hard_reset_valid := true.B
      dmi_sticky_error := false.B
      printf("dtmcs dmi hard reset\n")
    }
    val write_result = Wire(Valid(UInt(dtm_reg_max_width.W)))
    write_result.valid := true.B
    write_result.bits := wdata
    write_result
  }

  val dtm_dtmcs_read_func = (addr: UInt, reg: UInt) => {
    val new_dtm_dtmcs_filed = new DTMDTMCSFiled(reg(31, 0))

    val read_result = Wire(Valid(UInt(dtm_reg_max_width.W)))
    read_result.valid := true.B
    read_result.bits := new_dtm_dtmcs_filed.get_read_data(
      Mux(in_dmi_error, DbgPKG.DMI_OP_STATUS_BUSY.U(2.W), dtm_dmi_filed.op)
    )
    read_result
  }

  val dmi_write_func = (addr: UInt, reg: UInt, wdata: UInt) => {
    // TODO: implement DMI write logic
    val write_result = Wire(Valid(UInt(dtm_reg_max_width.W)))
    write_result.valid := true.B
    write_result.bits := wdata

    when(!in_dmi_error) {
      reg := write_result.bits
    }
    //  In Update-DR, the DTM starts the operation specified in op unless the current status reported in
    //  op is sticky
    can_dmi_send_req := !in_dmi_error
    write_result
  }

  val dmi_read_func = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(dtm_reg_max_width.W)))
    read_result.valid := true.B
    read_result.bits := dtm_dmi_filed.get_read_data(
      Mux(in_dmi_error, DbgPKG.DMI_OP_STATUS_BUSY.U(2.W), dtm_dmi_filed.op)
    )
    read_result
  }

  val normal_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(dtm_reg_max_width.W)))
    read_result.valid := true.B
    read_result.bits := reg
    read_result
  }
  val normal_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(dtm_reg_max_width.W)))
    write_result.valid := true.B
    write_result.bits := wdata
    reg := write_result.bits
    write_result
  }

  val empty_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(dtm_reg_max_width.W)))
    write_result.valid := true.B
    write_result.bits := 0.U
    write_result
  }

  val empty_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(dtm_reg_max_width.W)))
    read_result.valid := true.B
    read_result.bits := 0.U
    read_result
  }

  val csr_map = Seq(
    (
      DbgPKG.JtagDTM_DTMCS,
      dtm_dtmcs,
      dtm_dtmcs_read_func,
      dtm_dtmcs_write_func
    ),
    (
      DbgPKG.JtagDTM_DMI,
      dtm_dmi,
      dmi_read_func,
      dmi_write_func
    ),
    (
      DbgPKG.JtagDTM_IDCODE,
      dtm_idcode,
      normal_read,
      empty_write
    ),
    (
      DbgPKG.JtagDTM_BYPASS0,
      dtm_bypass0,
      empty_read,
      empty_write,
    ),
    (
      DbgPKG.JtagDTM_BYPASS1,
      dtm_bypass1,
      empty_read,
      empty_write,
    ),
    (
      DbgPKG.JtagDTM_CUSTOM,
      dmt_custom,
      normal_read,
      normal_write
    )
  )

  for ((addr, reg, read_func, write_func) <- csr_map) {
    dtm_csrs.add_csr(addr, reg, read_func, write_func)
  }

  // -------------------------
  // Jtag TAP Ir logic
  // -------------------------

  // IR shift register
  when(jtag_state === JtagState.ShiftIr) {
    ir_shift_reg.io.shift_valid := true.B
    ir_shift_reg.io.shift_in := io.jtag.tdi && io.jtag.tdi_en
  }

  // IR capture register
  when(jtag_state === JtagState.CaptureIr) {
    ir_shift_reg.io.update_valid := true.B
    ir_shift_reg.io.update_data := 1.U
    ir_shift_reg.io.update_data_len := dm_config.ir_length.U
  }

  // IR update register
  when(jtag_state === JtagState.UpdateIr) {
    ir_reg := ir_shift_reg.io.shift_reg_out
  }

  // synchronize test logic reset
  when(jtag_state === JtagState.TestLogicReset) {
    ir_reg := DbgPKG.JtagDTM_IDCODE.U
    ir_shift_reg.io.update_valid := true.B
    ir_shift_reg.io.update_data := 0.U
    ir_shift_reg.io.update_data_len := 1.U
  }

  // --------------------------
  // Jtag TAP Dr logic
  // --------------------------

  // Dr shift register
  when(jtag_state === JtagState.ShiftDr) {
    dr_shift_reg.io.shift_valid := true.B
    dr_shift_reg.io.shift_in := io.jtag.tdi && io.jtag.tdi_en
  }

  // Dr capture register
  when(jtag_state === JtagState.CaptureDr) {

    // if capture_dr goes high while we are in the read state
    // or in the corresponding wait state we are not giving back a valid word
    // -> throw an error
    when(dmi_busy && dmi_req_op_is_read) {
      dmi_sticky_error := true.B
    }

    dr_shift_reg.io.update_valid := true.B
    dr_shift_reg.io.update_data := dtm_csrs.read(ir_reg).bits
    dr_shift_reg.io.update_data_len := MuxLookup(
      ir_reg,
      1.U // default bypass
    ) {
      Seq(
        DbgPKG.JtagDTM_DTMCS.U -> 32.U,
        DbgPKG.JtagDTM_DMI.U -> dtm_reg_max_width.U,
        DbgPKG.JtagDTM_IDCODE.U -> 32.U,
        DbgPKG.JtagDTM_CUSTOM.U -> 32.U,
        DbgPKG.JtagDTM_BYPASS0.U -> 1.U,
        DbgPKG.JtagDTM_BYPASS1.U -> 1.U
      )
    }
  }
  // Dr update register
  when(jtag_state === JtagState.UpdateDr) {

    // update_dr means we got another request but we didn't finish
    // the one in progress, this state is sticky
    when(dmi_busy) {
      dmi_sticky_error := true.B
    }
    dtm_csrs.write(ir_reg, dr_shift_reg.io.shift_reg_out)
  }

  // --------------------------
  // TAP TDO logic
  // --------------------------
  withClock(clock_falling) {

    val tdo_en = WireInit(false.B)
    val tdo = WireInit(false.B)

    when(jtag_state === JtagState.ShiftIr) {
      tdo := ir_shift_reg.io.shift_out
      tdo_en := true.B
    }

    when(jtag_state === JtagState.ShiftDr) {
      tdo := dr_shift_reg.io.shift_out
      tdo_en := true.B
    }
    io.jtag.tdo := RegNext(tdo)
    io.jtag.tdo_en := RegNext(tdo_en)
  }

  // --------------------------
  // DMI logic
  // --------------------------

  val sDMIIdle :: sDMIReq :: sDMIResp :: Nil = Enum(3)

  val dmi_state = withReset(jtag_tap_reset) { RegInit(sDMIIdle) }
  val dmi_req_buf = withReset(jtag_tap_reset) {
    RegInit(0.U.asTypeOf(new DMIReq(dm_config.abits)))
  }

  when(jtag_tap_reset) {
    assert(
      dmi_state === sDMIIdle,
      "dmi_state should be sDMIIdle when jtag_tap_reset"
    )
  }

  switch(dmi_state) {
    is(sDMIIdle) {
      val is_dmi_nop = dtm_dmi_filed.op === DbgPKG.DMI_OP_NOP.U
      dontTouch(is_dmi_nop)
      when(can_dmi_send_req) {
        can_dmi_send_req := false.B
        when(!is_dmi_nop) {
          dmi_state := sDMIReq
          dmi_busy := true.B
          dmi_req_buf.op := dtm_dmi_filed.op
          dmi_req_buf.addr := dtm_dmi_filed.address
          dmi_req_buf.data := dtm_dmi_filed.data
          dmi_req_op_is_read := dtm_dmi_filed.op === DbgPKG.DMI_OP_READ.U
        }
      }
    }
    is(sDMIReq) {
      io.dmi_req.enq(dmi_req_buf)
      when(io.dmi_req.fire) {
        dmi_state := sDMIResp
      }
    }
    is(sDMIResp) {
      io.dmi_resp.ready := true.B
      when(io.dmi_resp.fire) {
        // In Capture-DR, the DTM updates data with the result from that operation, updating op if the
        // current op isn’t sticky.
        // (We actually update the dmi register here)

        dtm_dmi := Mux(
          dmi_sticky_error,
          Cat(
            io.dmi_resp.bits.addr,
            io.dmi_resp.bits.data,
            DbgPKG.DMI_OP_STATUS_BUSY.U(2.W)
          ),
          io.dmi_resp.bits.raw
        )

        dtm_dmi := io.dmi_resp.bits.raw
        dmi_state := sDMIIdle
        dmi_busy := false.B
      }
    }
  }
}

object GenJtagDTM_verilog extends App {
  val dm_config = new DebugModuleConfig()
  GenVerilogHelper(
    new JtagDTM(dm_config)
  )
}
