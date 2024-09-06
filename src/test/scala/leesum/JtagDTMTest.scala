package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import leesum.TestUtils.long2UInt64
import leesum.dbg.DbgPKG.DMI_OP_WRITE
import leesum.dbg._
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class JtagDTMTest extends AnyFreeSpec with ChiselScalatestTester {
  val dm_config = new DebugModuleConfig()

  def jtag_to_test_logic_reset(dut: JtagDTM): Unit = {
    // 5 cycles of tms = 1
    for (i <- 0 until 5) {
      dut.io.jtag.tms.poke(true.B)
      dut.clock.step(1)
    }
    dut.io.jtag_state.expect(JtagState.TestLogicReset)
  }

  def jtag_to_run_test_idle(dut: JtagDTM): Unit = {
    jtag_to_test_logic_reset(dut)
    dut.io.jtag.tms.poke(false.B)
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.RunTestIdle)
  }

  def jtag_ir_in_out(
      dut: JtagDTM,
      ir_in: UInt,
      ir_out: Long,
      width: Int
  ): Unit = {

    dut.io.jtag_state.expect(JtagState.RunTestIdle)

    dut.io.jtag.tms.poke(true.B) // select-DR-scan
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.SelectDrScan)

    dut.io.jtag.tms.poke(true.B) // select-IR-scan
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.SelectIrScan)

    dut.io.jtag.tms.poke(false.B) // capture-IR
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.CaptureIr)

    dut.io.jtag.tms.poke(false.B)
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.ShiftIr)

    var ir_out_shift: Long = 0
    // --------- shift-IR Loop Start ---------------
    for (i <- 0 until width - 1) {

      dut.io.jtag.tdi.poke(ir_in(i))
      dut.io.jtag.tdi_en.poke(true.B)
      dut.io.jtag.tms.poke(false.B)
      dut.clock.step(1)

      dut.io.jtag.tdo_en.expect(true.B)
      ir_out_shift |= dut.io.jtag.tdo.peekInt().toLong << i
      println(ir_out, ir_out_shift)

      dut.io.jtag_state.expect(JtagState.ShiftIr)
    }

    dut.io.jtag.tdi.poke(ir_in(width - 1)) // last bit
    dut.io.jtag.tdi_en.poke(true.B)

    dut.io.jtag.tms.poke(true.B) // exit1-IR
    dut.clock.step(1)
    dut.io.jtag.tdi_en.poke(false.B)
    dut.io.jtag.tdi.poke(false.B)

    dut.io.jtag.tdo_en.expect(true.B)
    ir_out_shift |= dut.io.jtag.tdo.peekInt().toLong << (width - 1)
    println(ir_out, ir_out_shift)
    assert(ir_out_shift == ir_out, "ir_out_shift != ir_out")

    dut.io.jtag_state.expect(JtagState.Exit1Ir)

    // --------- shift-IR Loop End ---------------

    dut.io.jtag.tms.poke(true.B) // update-IR
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.UpdateIr)

    dut.io.jtag.tms.poke(false.B) // run-test-idle
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.RunTestIdle)

    dut.io.jtag_ir.expect(ir_in)

  }

  def jtag_dr_in_out(
      dut: JtagDTM,
      data_in: UInt,
      data_out: Long,
      width: Int,
      check_out: Boolean = false
  ): Unit = {

    dut.io.jtag_state.expect(JtagState.RunTestIdle)
    dut.io.jtag.tms.poke(true.B) // select-DR-scan
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.SelectDrScan)

    dut.io.jtag.tms.poke(false.B) // capture-DR
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.CaptureDr)

    dut.io.jtag.tms.poke(false.B)
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.ShiftDr)

    var dr_out_shift: Long = 0
    // --------- shift-DR Loop Start ---------------
    for (i <- 0 until width - 1) {

      dut.io.jtag.tdi.poke(data_in(i))
      dut.io.jtag.tdi_en.poke(true.B)

      dut.io.jtag.tms.poke(false.B)
      dut.clock.step(1)

      dut.io.jtag.tdo_en.expect(true.B)
      dr_out_shift |= dut.io.jtag.tdo.peekInt().toLong << i

      dut.io.jtag_state.expect(JtagState.ShiftDr)
    }

    dut.io.jtag.tdi.poke(data_in(width - 1)) // last bit
    dut.io.jtag.tdi_en.poke(true.B)

    dut.io.jtag.tms.poke(true.B) // exit1-DR
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.Exit1Dr)

    dut.io.jtag.tdo_en.expect(true.B)
    dr_out_shift |= dut.io.jtag.tdo.peekInt().toLong << (width - 1)

    if (check_out) {
      assert(
        dr_out_shift.toHexString == data_out.toHexString,
        "dr_out_shift != data_out"
      )
    }

    dut.io.jtag.tdi.poke(false.B)
    dut.io.jtag.tdi_en.poke(false.B)

    // --------- shift-DR Loop End ---------------

    dut.io.jtag.tms.poke(true.B) // update-DR
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.UpdateDr)

    dut.io.jtag.tms.poke(false.B) // run-test-idle
    dut.clock.step(1)
    dut.io.jtag_state.expect(JtagState.RunTestIdle)

  }

  /** Generate DMI command for register access. Shall write this command to DMI
    * command register
    * @param regno
    *   register number
    * @param aarsize
    * @param write
    * @return
    */
  def gen_dmi_command_regtype(
      regno: Int,
      aarsize: Int,
      write: Boolean
  ) = {
    require(
      Seq(DbgPKG.AARSIZE_32, DbgPKG.AARSIZE_64).contains(
        aarsize
      ),
      "aarsize should be 32, 64"
    )
    val reg_cmd = new CSRBitField(0)
    reg_cmd.set_field(CommandRegMask.write, if (write) 1 else 0)
    reg_cmd.set_field(CommandRegMask.regno, regno)
    reg_cmd.set_field(CommandRegMask.cmdtype, DbgPKG.COMDTYPE_ACCESS_REG)
    reg_cmd.set_field(CommandRegMask.aarsize, aarsize)
    reg_cmd.get_raw
  }

  /** Generate DMI command for memory access. Shall write this command to DMI
    * command register
    * @param aamsize
    *   access size
    * @param write
    *   write or read
    * @return
    */
  def gen_dmi_command_memtype(
      aamsize: Int,
      write: Boolean
  ) = {
    require(
      Seq(
        DbgPKG.AAMSIZE_8,
        DbgPKG.AAMSIZE_16,
        DbgPKG.AAMSIZE_32,
        DbgPKG.AAMSIZE_64
      ).contains(
        aamsize
      ),
      "aamsize should be 8, 16, 32, 64"
    )
    val mem_cmd = new CSRBitField(0)
    mem_cmd.set_field(CommandMemMask.write, if (write) 1 else 0)
    mem_cmd.set_field(CommandMemMask.aamvirtual, 0) // physical address
    mem_cmd.set_field(CommandMemMask.aamsize, aamsize)
    mem_cmd.set_field(CommandMemMask.cmdtype, DbgPKG.COMDTYPE_ACCESS_MEM)
    mem_cmd.get_raw
  }

  /** Generate DTM_DMI register data
    * @param addr
    *   address
    * @param data
    *   data
    * @param op
    *   operation
    * @return
    */
  def gen_jtag_dtm_dmi_data(
      addr: Int,
      data: Long,
      op: Int
  ): Long = {
    require(addr < 32)
    require(
      Seq(DbgPKG.DMI_OP_READ, DbgPKG.DMI_OP_WRITE, DbgPKG.DMI_OP_NOP)
        .contains(op)
    )

    val dmi_reg_value = new CSRBitField(0)
    dmi_reg_value.set_field(DTMDMIMask.op, op)
    dmi_reg_value.set_field(DTMDMIMask.data, data)
    dmi_reg_value.set_field(DTMDMIMask.addr, addr)
    dmi_reg_value.get_raw
  }

  "JtagStateLogicResetTest" in {
    test(
      new JtagDTM(dm_config)
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.jtag.tms.poke(false.B)
        dut.io.jtag.tdi.poke(false.B)

        for (_ <- 0 until 20000) {
          // randomize tms
          for (i <- 0 until 50) {
            val random_bool = Gen.oneOf(true, false).sample.get
            dut.io.jtag.tms.poke(random_bool.B)
            dut.clock.step(1)
          }
          // 5 cycles of tms = 1
          jtag_to_test_logic_reset(dut)

          dut.io.jtag_state.expect(JtagState.TestLogicReset)
        }
      }
  }

  "JtagSelectIrTest" in {
    test(
      new JtagDTM(dm_config)
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.jtag.tms.poke(false.B)
        dut.io.jtag.tdi.poke(false.B)

        jtag_to_run_test_idle(dut)

        jtag_ir_in_out(dut, long2UInt64(0x1f), 0x1, 5)
      }
  }

  "JtagSelectDrTest" in {
    test(
      new JtagDTM(dm_config)
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.jtag.tms.poke(false.B)
        dut.io.jtag.tdi.poke(false.B)

        jtag_to_run_test_idle(dut)
        jtag_ir_in_out(dut, long2UInt64(0x01), 0x1, 5)

        // IDCODE should be 0xdeadbeef
        jtag_dr_in_out(
          dut,
          long2UInt64(0L),
          0xdeadbeefL,
          32,
          check_out = true
        )
      }
  }

  "JtagRWTest" in {
    test(
      new JtagDTM(dm_config)
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.jtag.tms.poke(false.B)
        dut.io.jtag.tdi.poke(false.B)

        jtag_to_run_test_idle(dut)

        // select custom register(can read and write)
        jtag_ir_in_out(dut, long2UInt64(DbgPKG.JtagDTM_CUSTOM), 0x1, 5)

        // write 0 to custom register
        jtag_dr_in_out(
          dut,
          long2UInt64(0x00),
          0,
          32
        )

        for (cur_val <- 1 until 20000) {
//          println(cur_val)
          // read back old value, write new value
          jtag_dr_in_out(
            dut,
            long2UInt64(cur_val),
            cur_val - 1,
            32,
            check_out = true
          )
        }

        jtag_to_test_logic_reset(dut)
        jtag_to_run_test_idle(dut)

        // IDCODE should be 0xdeadbeef
        jtag_dr_in_out(
          dut,
          long2UInt64(0x00),
          0xdeadbeefL,
          32,
          check_out = true
        )
      }
  }

  "JtagRWDMITest" in {
    test(
      new JtagDTM(dm_config)
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.jtag.tms.poke(false.B)
        dut.io.jtag.tdi.poke(false.B)

        jtag_to_run_test_idle(dut)

        // ir select DMI register
        jtag_ir_in_out(
          dut,
          long2UInt64(DbgPKG.JtagDTM_DMI),
          0x1,
          dm_config.ir_length
        )

        val dtm_dmi_data = gen_jtag_dtm_dmi_data(
          DbgPKG.ABSTRACT_DATA_BASE,
          0x12adbeefL,
          DMI_OP_WRITE
        )
        jtag_dr_in_out(
          dut,
          long2UInt64(dtm_dmi_data),
          0,
          34 + dm_config.abits
        )

        println(dtm_dmi_data.toHexString)

        // write 0x12adbeef to DMI register
        dut.io.dmi_req.expectDequeue(
          new DMIReq(dm_config.abits).Lit(
            _.addr -> DbgPKG.ABSTRACT_DATA_BASE.U,
            _.data -> 0x12adbeefL.U,
            _.op -> DMI_OP_WRITE.U
          )
        )

        dut.io.dmi_resp.enqueue(
          new DMIResp(dm_config.abits).Lit(
            _.addr -> DbgPKG.ABSTRACT_DATA_BASE.U,
            _.data -> 0xdeadbeefL.U,
            _.op -> DbgPKG.DMI_OP_STATUS_SUCCESS.U
          )
        )
        dut.clock.step(5)

        // read back 0x12ad34ef from DMI register

        val read_back_dmi_data = gen_jtag_dtm_dmi_data(
          DbgPKG.ABSTRACT_DATA_BASE,
          0xdeadbeefL,
          DbgPKG.DMI_OP_STATUS_SUCCESS
        )

        jtag_dr_in_out(
          dut,
          long2UInt64(0x00),
          read_back_dmi_data,
          34 + dm_config.abits,
          check_out = true
        )

      }
  }
}
