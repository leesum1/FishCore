package leesum

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.{DecoupledIO, ValidIO}
import chiseltest._
import leesum.Cache.{DCacheConnect, DummyDCache}
import leesum.axi4.AXI4Memory
import leesum.dbg.DbgPKG._
import leesum.dbg.{
  CommandRegMask,
  DMIReq,
  DMIResp,
  DbgPKG,
  DbgSlaveState,
  DebugModule,
  DebugModuleConfig
}
import org.scalatest.freespec.AnyFreeSpec

class DebugModuleTestDut(dm_config: DebugModuleConfig) extends Module {
  val io = IO(new Bundle {
    //  dm <> core interface
    val debug_state_regs = Input(new DbgSlaveState())
    val debug_halt_req = Output(ValidIO(Bool()))
    val debug_resume_req = Output(ValidIO(Bool()))
    val debug_reset_req = Output(ValidIO(Bool()))
    val debug_clear_havereset = Output(ValidIO(Bool()))
    // dmi <> dm interface
    val dmi_req = Flipped(DecoupledIO(new DMIReq(dm_config.abits)))
    val dmi_resp = DecoupledIO(new DMIResp(dm_config.abits))
  })

  val debug_module = Module(new DebugModule(dm_config))
  val gprs = Module(new GPRs(1, 1))
  val csrs = Module(new CSRRegs(1, 1))
  val axi4mem = Module(
    new AXI4Memory(
      32, 64, 0x1000_000L, 64, 0x8000_0000L
    )
  )
  val dummy_dcache = Module(new DummyDCache())

  csrs.io.direct_write_ports.clear()
  csrs.io.cur_privilege_mode := 0.U
  dummy_dcache.io.flush := false.B

  csrs.io.mtime := 0.U
  csrs.io.time_int := false.B
  csrs.io.soft_int := false.B
  csrs.io.mext_int := false.B
  csrs.io.sext_int := false.B

  io.dmi_req <> debug_module.io.dmi_req
  io.dmi_resp <> debug_module.io.dmi_resp

  io.debug_halt_req <> debug_module.io.debug_halt_req
  io.debug_resume_req <> debug_module.io.debug_resume_req
  io.debug_reset_req <> debug_module.io.debug_reset_req
  io.debug_clear_havereset <> debug_module.io.debug_clear_havereset
  debug_module.io.debug_state_regs <> io.debug_state_regs

  debug_module.io.debug_gpr_write_port <> gprs.io.write_ports(0)
  debug_module.io.debug_gpr_read_port <> gprs.io.read_ports(0)
  debug_module.io.debug_csr_write_port <> csrs.io.write_ports(0)
  debug_module.io.debug_csr_read_port <> csrs.io.read_ports(0)

  dummy_dcache.io.axi_mem <> axi4mem.io

  DCacheConnect.dcache_req_to_load_store_req(
    debug_module.io.debug_dcache_req,
    dummy_dcache.io.load_req,
    dummy_dcache.io.store_req
  )

  val last_is_store = RegInit(false.B)
  DCacheConnect.dcache_resp_to_load_store_resp(
    last_is_store,
    dummy_dcache.io.load_resp,
    dummy_dcache.io.store_resp,
    debug_module.io.debug_dcache_resp
  )

  when(dummy_dcache.io.store_req.fire) {
    last_is_store := true.B
  }.elsewhen(dummy_dcache.io.load_req.fire) {
    last_is_store := false.B
  }

}

class DebugModuleTest extends AnyFreeSpec with ChiselScalatestTester {
  val dm_config = new DebugModuleConfig()

  def gen_dmi_req(addr: UInt, data: UInt, op: UInt) = {
    new DMIReq(dm_config.abits).Lit(
      _.addr -> addr,
      _.data -> data,
      _.op -> op
    )
  }

  def gen_dmi_resp(addr: UInt, data: UInt, op: UInt) = {
    new DMIResp(dm_config.abits).Lit(
      _.addr -> addr,
      _.data -> data,
      _.op -> op
    )
  }

  /** generate dmi request and response for r/w abstract data
    * @param idx
    *   idx of abstract data
    * @param expect_data
    * @param is_write
    * @return
    *   (dmi_req_absdata, dmi_resp_absdata)
    */
  def gen_dmi_absdata(idx: Int, expect_data: Long, is_write: Boolean) = {
    val dmi_req_absdata = gen_dmi_req(
      (DbgPKG.ABSTRACT_DATA_BASE + idx).U,
      if (is_write) { Long2UInt32(expect_data) }
      else { 0.U },
      if (is_write) { DMI_OP_WRITE.U }
      else { DMI_OP_READ.U }
    )

    val dmi_resp_absdata = gen_dmi_resp(
      (DbgPKG.ABSTRACT_DATA_BASE + idx).U,
      Long2UInt32(expect_data),
      DMI_OP_STATUS_SUCCESS.U
    )
    (dmi_req_absdata, dmi_resp_absdata)
  }

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
    reg_cmd.setField(CommandRegMask.write, if (write) 1 else 0)
    reg_cmd.setField(CommandRegMask.regno, regno)
    reg_cmd.setField(CommandRegMask.cmdtype, DbgPKG.COMDTYPE_ACCESS_REG)
    reg_cmd.setField(CommandRegMask.aarsize, aarsize)

    val command_dmi_req =
      gen_dmi_req(
        DbgPKG.COMMAND_ADDR.U,
        reg_cmd.getRawValue.U(32.W),
        DMI_OP_WRITE.U
      )
    val command_dmi_resp =
      gen_dmi_resp(
        DbgPKG.COMMAND_ADDR.U,
        reg_cmd.getRawValue.U(32.W),
        DMI_OP_STATUS_SUCCESS.U
      )
    (command_dmi_req, command_dmi_resp)
  }

  /** generate abstract command to r/w register
    * @param addr
    * @param aarsize_val
    * @param is_write
    * @return
    */
  def gen_abs_reg_cmd(
      addr: Int,
      aarsize_val: Int,
      is_write: Boolean = false
  ): UInt = {
    require(
      Seq(DbgPKG.AARSIZE_32, DbgPKG.AARSIZE_64, DbgPKG.AARSIZE_128).contains(
        aarsize_val
      ),
      "aarsize_val should be 32, 64, 128"
    )

    val reg_cmd = new CSRBitField(0)
    reg_cmd.setField(CommandRegMask.write, if (is_write) 1 else 0)
    reg_cmd.setField(CommandRegMask.regno, addr)
    reg_cmd.setField(CommandRegMask.cmdtype, DbgPKG.COMDTYPE_ACCESS_REG)
    reg_cmd.setField(CommandRegMask.aarsize, aarsize_val)
    reg_cmd.getRawValue.U(32.W)
  }

  "DMI hs test" in {
    test(new DebugModule(dm_config))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // -------------------
        // init ports
        // -------------------

        dut.clock.step(10)

        dut.io.dmi_req.enqueue(
          new DMIReq(6)
            .Lit(
              _.addr -> PROGBUF_BASE.U,
              _.data -> 0xffdd.U,
              _.op -> DMI_OP_WRITE.U
            )
        )

        dut.clock.step(5)

        dut.io.dmi_resp.expectDequeue(
          new DMIResp(6)
            .Lit(
              _.addr -> PROGBUF_BASE.U,
              _.data -> 0xffdd.U,
              _.op -> DMI_OP_STATUS_SUCCESS.U
            )
        )

        dut.io.dmi_req.enqueue(
          new DMIReq(6)
            .Lit(
              _.addr -> PROGBUF_BASE.U,
              _.data -> 0.U,
              _.op -> DMI_OP_READ.U
            )
        )

        dut.io.dmi_resp.expectDequeue(
          new DMIResp(6)
            .Lit(
              _.addr -> PROGBUF_BASE.U,
              _.data -> 0xffdd.U,
              _.op -> DMI_OP_STATUS_SUCCESS.U
            )
        )

        dut.io.dmi_req.enqueue(
          new DMIReq(6)
            .Lit(
              _.addr -> 0.U,
              _.data -> 0.U,
              _.op -> DMI_OP_READ.U
            )
        )

        dut.io.dmi_resp.expectDequeue(
          new DMIResp(6)
            .Lit(
              _.addr -> 0.U,
              _.data -> 0.U,
              _.op -> DMI_OP_STATUS_FAILED.U
            )
        )

      }
  }

  "Debug RW Reg Test" in {
    test(new DebugModuleTestDut(dm_config))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.clock.step(5)

        // halt core
        dut.io.debug_state_regs.is_halted.poke(true.B)

        // -------------------
        // 1. write register
        // -------------------

        // 1. write wdata to absdata
        val write_absdata_dmi =
          gen_dmi_absdata(0, 0xdeadbeef, is_write = true)
        val write_absdata_dmi1 =
          gen_dmi_absdata(1, 0xbed6efe8, is_write = true)
        fork {
          dut.io.dmi_req.enqueue(write_absdata_dmi._1)
          dut.io.dmi_req.enqueue(write_absdata_dmi1._1)
        }.fork {
          dut.io.dmi_resp.expectDequeue(write_absdata_dmi._2)
          dut.io.dmi_resp.expectDequeue(write_absdata_dmi1._2)
        }.joinAndStep()
        // 2. use abstract command to write register
        val write_reg_cmd_dmi32 =
          gen_dmi_command_regtype(0x1000 + 4, DbgPKG.AARSIZE_32, write = true)
        val write_reg_cmd_dmi64 =
          gen_dmi_command_regtype(0x1000 + 5, DbgPKG.AARSIZE_64, write = true)
        fork {
          dut.io.dmi_req.enqueue(write_reg_cmd_dmi32._1)
          dut.io.dmi_req.enqueue(write_reg_cmd_dmi64._1)
        }.fork {
          dut.io.dmi_resp.expectDequeue(write_reg_cmd_dmi32._2)
          dut.io.dmi_resp.expectDequeue(write_reg_cmd_dmi64._2)
        }.joinAndStep()

        dut.clock.step(5)

        // -------------------
        // 2. read register
        // -------------------

        // use abstract command to read register
        val read_reg_cmd_dmi32 =
          gen_dmi_command_regtype(0x1000 + 4, DbgPKG.AARSIZE_32, write = false)
        val read_reg_cmd_dmi64 =
          gen_dmi_command_regtype(0x1000 + 5, DbgPKG.AARSIZE_64, write = false)

        val write_absdata_dmi_zero_seq = Seq(
          gen_dmi_absdata(0, 0, is_write = true),
          gen_dmi_absdata(1, 0, is_write = true)
        )

        val read_absdata_dmi_seq = Seq(
          gen_dmi_absdata(0, 0xdeadbeef, is_write = false),
          gen_dmi_absdata(1, 0xbed6efe8, is_write = false)
        )

        def clear_absdata() = {
          fork {
            write_absdata_dmi_zero_seq.foreach { case (req, resp) =>
              dut.io.dmi_req.enqueue(req)
              dut.io.dmi_resp.expectDequeue(resp)
            }
          }.joinAndStep()
        }
        // clear absdata
        clear_absdata()
        // send abstract command to read register
        dut.io.dmi_req.enqueue(read_reg_cmd_dmi32._1)
        dut.io.dmi_resp.expectDequeue(read_reg_cmd_dmi32._2)
        // read absdata to check
        dut.io.dmi_req.enqueue(read_absdata_dmi_seq(0)._1)
        dut.io.dmi_resp.expectDequeue(read_absdata_dmi_seq(0)._2)

        // clear absdata
        clear_absdata()
        // send abstract command to read register
        dut.io.dmi_req.enqueue(read_reg_cmd_dmi64._1)
        dut.io.dmi_resp.expectDequeue(read_reg_cmd_dmi64._2)
        // read absdata to check
        dut.io.dmi_req.enqueue(read_absdata_dmi_seq(0)._1)
        dut.io.dmi_resp.expectDequeue(read_absdata_dmi_seq(0)._2)
        dut.io.dmi_req.enqueue(read_absdata_dmi_seq(1)._1)
        dut.io.dmi_resp.expectDequeue(read_absdata_dmi_seq(1)._2)

        dut.clock.step(5)

      }
  }
}
