package leesum.dbg

import chisel3._
import chisel3.aop.Select.When
import chisel3.util.{DecoupledIO, _}
import leesum.Cache.{DCacheReq, DCacheResp}
import leesum.Utils.SimLog
import leesum._
import leesum.axi4.AXIDef

class DebugModuleConfig {
  val progbuf_num = 0
  val abstract_data_num = 6
  val abits = 6
  val ir_length = 5
  val dm_base_addr = 0x0

}

class DMIReq(abits: Int) extends Bundle {
  val addr = UInt(abits.W)
  val data = UInt(32.W)
  val op = UInt(2.W)

  def raw: UInt = Cat(addr, data, op)
}

class DMIResp(abits: Int) extends DMIReq(abits) {}

class DebugModuleCoreInterface extends Bundle {
  val state_regs = Input(new DbgSlaveState())
  val halt_req = Output(ValidIO(Bool()))
  val resume_req = Output(ValidIO(Bool()))
  val reset_req = Output(ValidIO(Bool()))
  val clear_havereset = Output(ValidIO(Bool()))
  val gpr_read_port = new RegFileReadPort
  val gpr_write_port = Flipped(new GPRsWritePort)
  val csr_read_port = new CSRReadPort
  val csr_write_port = new CSRWritePort
  val dcache_req = Decoupled(new DCacheReq)
  val dcache_resp = Flipped(Decoupled(new DCacheResp))

  def clear_port_as_master(): Unit = {
    halt_req.valid := false.B
    resume_req.valid := false.B
    reset_req.valid := false.B
    clear_havereset.valid := false.B
    halt_req.bits := false.B
    resume_req.bits := false.B
    reset_req.bits := false.B
    clear_havereset.bits := false.B
    gpr_read_port.rs1_addr := DontCare
    gpr_read_port.rs2_addr := DontCare
    gpr_write_port.wen := false.B
    gpr_write_port.addr := DontCare
    gpr_write_port.wdata := DontCare
    csr_read_port.addr := DontCare
    csr_read_port.read_en := false.B
    csr_write_port.addr := DontCare
    csr_write_port.write_data := DontCare
    csr_write_port.write_en := false.B
    dcache_req.noenq()
    dcache_req.bits.id := 3.U
    dcache_resp.nodeq()
  }
}

/** Debug Module works in system clock domain
  * 1.Debug Module receives DMI request from JTAG DTM And sends DMI response to
  * JTAG DTM. 2.Debug Module communicates with core, including R/W GPRs, R/W
  * CSRs, R/W Memory and so on
  * @param dm_config
  */
class DebugModule(dm_config: DebugModuleConfig) extends Module {
  val io = IO(new Bundle {

    //  dm <> core interface
    val debug_core_interface = new DebugModuleCoreInterface

    // dmi <> dm interface
    val dmi_req = Flipped(DecoupledIO(new DMIReq(dm_config.abits)))
    val dmi_resp = DecoupledIO(new DMIResp(dm_config.abits))
  })

  io.debug_core_interface.clear_port_as_master()
  io.dmi_req.nodeq();
  io.dmi_resp.noenq();

  // ------------------------------
  // debug module registers
  // 1. dmstatus should have an init value
  // 2. abstractcs should have an init value
  // ------------------------------
  val dmstatus_init_val = new CSRBitField(0)
  val abstractcs_init_val = new CSRBitField(0)

  // There is a Debug Module, and it conforms to version 0.13 of this specification.
  dmstatus_init_val.set_field(DMStatusMask.version, 2)
  // On components that don’t implement authentication, this bit must be preset as 1.
  dmstatus_init_val.set_field(DMStatusMask.authenticated, 1)
  dmstatus_init_val.set_field(DMStatusMask.impebreak, 0)

  abstractcs_init_val.set_field(
    AbstractcsMask.datacount,
    dm_config.abstract_data_num
  )
  // Don’t support program buffer access
  abstractcs_init_val.set_field(AbstractcsMask.progbufsize, 0)

  val progbuf = RegInit(
    VecInit(Seq.fill(math.max(1, dm_config.progbuf_num))(0.U(32.W)))
  )
  val abstract_data = RegInit(
    VecInit(Seq.fill(dm_config.abstract_data_num)(0.U(32.W)))
  )
  val dmcontrol = RegInit(0.U(32.W))
  val dmstatus = RegInit(dmstatus_init_val.get_raw.U(32.W))
  val hartinfo = RegInit(0.U(32.W))
  val abstractcs = RegInit(abstractcs_init_val.get_raw.U(32.W))
  val command = RegInit(0.U(32.W))
  val sbcs = RegInit(0.U(32.W)) // not support sbcs, always 0

  def arg_read32(idx: Int): UInt = {
    require(idx < dm_config.abstract_data_num)
    abstract_data(idx)
  }

  def arg_write32(idx: Int, data: UInt): Unit = {
    require(idx < dm_config.abstract_data_num)
    require(data.getWidth == 32)
    abstract_data(idx) := data
  }

  def arg_read64(idx: Int): UInt = {
    require(idx < dm_config.abstract_data_num / 2)
    val r64 = Cat(abstract_data(idx * 2 + 1), abstract_data(idx * 2))
    require(r64.getWidth == 64)
    r64
  }
  def arg_write64(idx: Int, data: UInt): Unit = {
    require(idx < dm_config.abstract_data_num / 2)
    require(data.getWidth == 64)
    abstract_data(idx * 2) := data(31, 0)
    abstract_data(idx * 2 + 1) := data(63, 32)
  }

  // -------------------------
  // dmcotrol r/w func define
  // -------------------------
  val dmcontrol_write = (_addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(32.W)))

    val old_dmcontrol_field = new DMControlFiled(reg)
    val new_dmcontrol_field = new DMControlFiled(wdata)

    when(new_dmcontrol_field.dmactive && !old_dmcontrol_field.dmactive) {
      SimLog.custom("DM", "dmactive is set to 1, reset debug module \n")
    }

    when(new_dmcontrol_field.ackhavereset) {
      io.debug_core_interface.clear_havereset.valid := true.B
    }

    // Writing 0 clears the halt request bit for all currently selected harts.
    // This may cancel outstanding halt requests for those harts.
    // Writing 1 sets the halt request bit for all currently
    // selected harts. Running harts will halt whenever
    // their halt request bit is set.
    io.debug_core_interface.halt_req.valid := true.B
    io.debug_core_interface.halt_req.bits := new_dmcontrol_field.haltreq

    when(new_dmcontrol_field.haltreq) {

      SimLog.custom(desiredName, "DM: hart haltreq\n")
    }.elsewhen(new_dmcontrol_field.resumereq) {
      // Writing 1 causes the currently selected harts to
      // resume once, if they are halted when to write
      // occurs. It also clears the resume ack bit for those
      // harts.
      // resumereq is ignored if haltreq is set.
      io.debug_core_interface.resume_req.valid := true.B
      SimLog.custom(desiredName, "DM: hart resumereq\"\n")
    }

    when(new_dmcontrol_field.hartreset) {
      // This optional field writes the reset bit for all the
      // currently selected harts. To perform a reset the
      // debugger writes 1, and then writes 0 to deassert
      // the reset signal.
      io.debug_core_interface.reset_req.valid := true.B
    }

    when(new_dmcontrol_field.ndmreset) {
      // This bit controls the reset signal from the DM
      // to the rest of the hardware platform. The signal
      // should reset every part of the hardware platform,
      // including every hart, except for the DM and any
      // logic required to access the DM
      // TODO: only support reset harts
      io.debug_core_interface.reset_req.valid := true.B
    }

    write_result.valid := true.B
    write_result.bits := new_dmcontrol_field.raw
    reg := wdata
    write_result
  }

  val dmcontrol_read = (_addr: UInt, reg: UInt) => {
    val dmcontrol_field = new DMControlFiled(reg)
    val read_result = Wire(Valid(UInt(32.W)))
    read_result.valid := true.B
    read_result.bits := dmcontrol_field.get_read_data(
      io.debug_core_interface.state_regs.have_reset()
    )
    read_result
  }
  // -------------------------
  // dmstatus r/w func define
  // -------------------------

  val dmstatus_read = (_addr: UInt, reg: UInt) => {
    val dmstatus_field = new DMStatusFiled(reg)
    val read_result = Wire(Valid(UInt(32.W)))
    read_result.valid := true.B
    read_result.bits := dmstatus_field.get_read_data(
      io.debug_core_interface.state_regs
    )
    read_result
  }

  // -------------------------
  // abstractcs r/w func define
  // -------------------------
  val abstractcs_write = (_addr: UInt, reg: UInt, wdata: UInt) => {
    val abstractcs_field = new AbstractcsFiled(reg)
    val write_result = Wire(Valid(UInt(32.W)))
    write_result.valid := true.B
    write_result.bits := abstractcs_field.get_new_write_data(wdata)
    reg := write_result.bits
    write_result
  }

  // -------------------------
  // command r/w func define
  // -------------------------
  val command_write = (_addr: UInt, reg: UInt, wdata: UInt) => {
    val command_field = new CommandFiled(reg)
    val write_result = Wire(Valid(UInt(32.W)))
    write_result.valid := true.B
    write_result.bits := command_field.raw
    reg := wdata
    write_result
  }

  val dm_map = new CSRMap()
  // -------------------------
  // map normal read write func define
  // -------------------------
  val normal_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(32.W)))
    read_result.valid := true.B
    read_result.bits := reg
    read_result
  }
  val normal_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(32.W)))
    write_result.valid := true.B
    write_result.bits := wdata
    reg := write_result.bits
    write_result
  }

  val empty_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(32.W)))
    write_result.valid := true.B
    write_result.bits := 0.U
    write_result
  }

  val empty_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(32.W)))
    read_result.valid := true.B
    read_result.bits := 0.U
    read_result
  }
  // add progbuf to map
  for (i <- 0 until dm_config.progbuf_num) {
    dm_map.add_csr(
      DbgPKG.PROGBUF_BASE + i,
      progbuf(i),
      normal_read,
      normal_write
    )
  }
  // add abstract_data to map
  for (i <- 0 until dm_config.abstract_data_num) {
    dm_map.add_csr(
      DbgPKG.ABSTRACT_DATA_BASE + i,
      abstract_data(i),
      normal_read,
      normal_write
    )
  }

  // DMSTATUS_ADDR
  val m_map =
    Seq(
      (DbgPKG.DMCONTROL_ADDR, dmcontrol, dmcontrol_read, dmcontrol_write),
      (DbgPKG.DMSTATUS_ADDR, dmstatus, dmstatus_read, empty_write),
      (DbgPKG.HARTINFO_ADDR, hartinfo, normal_read, normal_write),
      (DbgPKG.ABSTRACTCS_ADDR, abstractcs, normal_read, abstractcs_write),
      (DbgPKG.COMMAND_ADDR, command, empty_read, command_write),
      (DbgPKG.HALTSUM1_ADDR, 0.U, empty_read, empty_write),
      (DbgPKG.HAWINDOWSEL_ADDR, 0.U, empty_read, empty_write),
      (DbgPKG.HAWINDOW, 0.U, empty_read, empty_write),
      (DbgPKG.ABSTRACTAUTO_ADDR, 0.U, empty_read, empty_write),
      (DbgPKG.SBCS_ADDR, sbcs, empty_read, empty_write)
    )

  for ((addr, reg, read_func, write_func) <- m_map) {
    dm_map.add_csr(addr, reg, read_func, write_func)
  }

  val sDMI_IDLE :: sDMI_PROC :: sDMI_SenResp :: sDMI_SideEff :: Nil = Enum(4)
  val dmi_state = RegInit(sDMI_IDLE)

  val dmi_req_buf = RegInit(0.U.asTypeOf(new DMIReq(dm_config.abits)))
  val dmi_resp_buf = RegInit(0.U.asTypeOf(new DMIResp(dm_config.abits)))

  val dmi_can_exit_side_effect_state = WireDefault(false.B)

  switch(dmi_state) {
    is(sDMI_IDLE) {
      io.dmi_req.ready := true.B
      when(io.dmi_req.fire) {
        dmi_req_buf := io.dmi_req.bits
        dmi_state := sDMI_PROC
      }
    }
    is(sDMI_PROC) {
      switch(dmi_req_buf.op) {
        is(DbgPKG.DMI_OP_READ.U) {
          // 读操作，读操作可以立马返回, 不会有副作用
          val read_result = dm_map.read(dmi_req_buf.addr, use_one_hot = true)
          dmi_resp_buf.addr := dmi_req_buf.addr
          dmi_resp_buf.data := read_result.bits
          dmi_resp_buf.op := Mux(
            read_result.valid,
            DbgPKG.DMI_OP_STATUS_SUCCESS.U,
            DbgPKG.DMI_OP_STATUS_FAILED.U
          )
          dmi_state := sDMI_SenResp
        }
        is(DbgPKG.DMI_OP_WRITE.U) {
          // 写操作，写操作可能会有副作用
          // 1. 写 command 寄存器 （执行 perform_abstract_command）
          val side_effect_dm_regs = VecInit(
//            DbgPKG.DMCONTROL_ADDR.asUInt,
            DbgPKG.COMMAND_ADDR.asUInt
          )

          val write_result = dm_map.write(dmi_req_buf.addr, dmi_req_buf.data)
          val on_write_side_effect =
            side_effect_dm_regs.contains(dmi_req_buf.addr)

          dmi_resp_buf.addr := dmi_req_buf.addr
          dmi_resp_buf.data := dmi_req_buf.data
          dmi_resp_buf.op := MuxCase(
            DbgPKG.DMI_OP_STATUS_FAILED.U,
            Seq(
//              (on_write_side_effect) -> DbgPKG.DMI_OP_STATUS_BUSY.U, TODO: 要不要进入到 busy 状态 ?
              (write_result.valid) -> DbgPKG.DMI_OP_STATUS_SUCCESS.U
            )
          )

          dmi_state := Mux(
            on_write_side_effect,
            sDMI_SideEff,
            sDMI_SenResp
          )
        }
        is(DbgPKG.DMI_OP_NOP.U) {
          dmi_state := sDMI_SenResp
        }
        is(DbgPKG.DMI_OP_RESERVED.U) {
          assert(false.B, "dmi reserved op")
        }
      }
    }
    is(sDMI_SenResp) {
      io.dmi_resp.valid := true.B
      io.dmi_resp.bits := dmi_resp_buf
      when(io.dmi_resp.fire) {
        dmi_state := sDMI_IDLE
      }
    }
    is(sDMI_SideEff) {
      // Wait for the side effect(abstract command) to complete
      when(dmi_can_exit_side_effect_state) {
        dmi_state := sDMI_SenResp
      }
    }
  }

  val need_perform_abstract_command = dmi_state === sDMI_SideEff

  dontTouch(need_perform_abstract_command)

  // 须要解析 command 寄存器
  val command_field = new CommandFiled(command)
  val abstractcs_field = new AbstractcsFiled(abstractcs)

  def set_abstractcs_busy_cmderr(cmderr_value: UInt, busy_value: Bool): Unit = {
    abstractcs := abstractcs_field.set_busy_and_cmderr_value(
      cmderr_value,
      busy_value
    )
  }

  // 1. 读取 gpr 寄存器: 不需要握手, 固定周期
  // 2. 读取 csr 寄存器: 需要握手, 不固定周期
  // 3. 读取 dcache: 需要握手, 不固定周期
  val sPerfABS_IDLE :: sPerfABS_REGReq :: sPerfABS_REGResp :: sPerfABS_MEMReq :: sPerfABS_MEMResp :: sPerfABS_ERR :: Nil =
    Enum(6)

  val perf_abs_state = RegInit(sPerfABS_IDLE)
  val perf_abs_result_buf = RegInit(0.U(64.W))
  val cmderr_buf = RegInit(0.U(3.W))

  val abs_mem_addr = arg_read64(1)
  val abs_mem_wdata = arg_read64(0)
  val abs_mem_size_buf = RegInit(0.U(2.W))
  val abs_mem_is_write = RegInit(false.B)

//  dontTouch(perf_abs_state)
  val supported_cmdtype = VecInit(
    DbgPKG.COMDTYPE_ACCESS_REG.U,
    DbgPKG.COMDTYPE_ACCESS_MEM.U
  )

  val not_supported_cmd_inst = VecInit(
    supported_cmdtype.contains(
      command_field.cmdtype
    ) === false.B, // not support cmdtype
    command_field.cmdtype === DbgPKG.COMDTYPE_ACCESS_REG.U && command_field.reg_field.aarsize === DbgPKG.AARSIZE_128.U, // not support 128 bit reg access
    command_field.cmdtype === DbgPKG.COMDTYPE_ACCESS_MEM.U && command_field.mem_field.aamsize === DbgPKG.AAMSIZE_128.U, // not support 128 bit mem access
    command_field.cmdtype === DbgPKG.COMDTYPE_ACCESS_MEM.U && command_field.mem_field.aamvirtual, // not support virtual mem access
    command_field.cmdtype === DbgPKG.COMDTYPE_ACCESS_MEM.U && command_field.mem_field.aampostincrement // not support aampostincrement
  ).reduce(_ || _)

  switch(perf_abs_state) {
    is(sPerfABS_IDLE) {

      // 1. 根据 command 寄存器的 cmdtype 字段, 执行不同的操作
      when(need_perform_abstract_command) {
        when(abstractcs_field.cmderr =/= DbgPKG.CMDERR_NONE.U) {
          // No abstract command is started until the value is reset to 0.
          // If cmderr is non-zero, writes to this register are ignored.
          SimLog.custom(
            desiredName,
            "Do not perform command when cmderr is not NONE, cmderr: %d\n",
            abstractcs_field.cmderr
          )
          // let dmi state machine continue
          dmi_can_exit_side_effect_state := true.B
        }.elsewhen(io.debug_core_interface.state_regs.running()) {
          // The abstract command couldn’t
          // execute because the hart wasn’t in the required
          // state (running/halted), or unavailable.

          // If an abstract command is started while the selected hart is unavailable or if a hart becomes
          // unavailable while executing an abstract command, then the Debug Module may terminate the abstract
          // command, setting busy low, and cmderr to 4 (halt/resume). Alternatively, the command could just
          // appear to be hung (busy never goes low).
          SimLog(
            desiredName,
            "Do not perform command when hart is running\n"
          )

          assert(
            io.debug_core_interface.state_regs.is_halted,
            "Do not perform command when hart is running"
          )
          cmderr_buf := DbgPKG.CMDERR_HALT_RESUME.U
          perf_abs_state := sPerfABS_ERR

        }.elsewhen(not_supported_cmd_inst) {
          // not support cmd
          SimLog(
            desiredName,
            "Do not support cmdtype: %d\n",
            command_field.cmdtype
          )

          cmderr_buf := DbgPKG.CMDERR_NOTSUP.U
          perf_abs_state := sPerfABS_ERR
        }.otherwise {
          // perform abstract command
          set_abstractcs_busy_cmderr(DbgPKG.CMDERR_NONE.U(3.W), true.B)

          val next_perf_abs_state = MuxLookup(
            command_field.cmdtype,
            sPerfABS_ERR
          ) {
            Seq(
              DbgPKG.COMDTYPE_ACCESS_REG.U -> sPerfABS_REGReq,
              DbgPKG.COMDTYPE_ACCESS_MEM.U -> sPerfABS_MEMReq
            )
          }

          assert(
            next_perf_abs_state =/= sPerfABS_ERR,
            "not support cmdtype"
          )

          perf_abs_state := next_perf_abs_state
        }

      }
    }
    is(sPerfABS_REGReq) {
      // 读取寄存器
      val reg_field = command_field.reg_field
      val w_regdata = Mux1H(
        Seq(
          (reg_field.aarsize === DbgPKG.AARSIZE_32.U) -> Cat(
            0.U(32.W),
            arg_read32(0)
          ),
          (reg_field.aarsize === DbgPKG.AARSIZE_64.U) -> arg_read64(0)
        )
      )

      val cmd_not_support = VecInit(
        (reg_field.is_gpr || reg_field.is_csr) === false.B, // 只能是 gpr 或 csr
        reg_field.aarsize === DbgPKG.AARSIZE_128.U // 不支持 128 位
      ).reduce(_ || _)

      assert(
        cmd_not_support === false.B,
        "not supported reg access addr or size"
      )

      // -------------------
      // 读写 gpr
      // -------------------
      when(command_field.reg_field.is_gpr) {
        when(command_field.reg_field.write) {
          // 写 gpr, size 在前面已经处理过了
          io.debug_core_interface.gpr_write_port.wen := true.B
          io.debug_core_interface.gpr_write_port.addr := command_field.reg_field.get_gpr_regno
          io.debug_core_interface.gpr_write_port.wdata := w_regdata

          SimLog(
            desiredName,
            "GPR Write addr:%d, wdata:%x\n",
            command_field.reg_field.get_gpr_regno,
            w_regdata
          )

        }.otherwise {
          // 读取 gpr，读取的时候不在乎 32 位还是 64 位
          io.debug_core_interface.gpr_read_port.rs1_addr := command_field.reg_field.get_gpr_regno
          perf_abs_result_buf := io.debug_core_interface.gpr_read_port.rs1_data

          SimLog(
            desiredName,
            "GPR Read addr:%d, rdata:%x\n",
            command_field.reg_field.get_gpr_regno,
            io.debug_core_interface.gpr_read_port.rs1_data
          )
        }

      }

      // -------------------
      // 读写 csr
      // -------------------

      when(command_field.reg_field.is_csr) {
        when(command_field.reg_field.write) {
          // 写 csr，size 在前面已经处理过了
          io.debug_core_interface.csr_write_port.addr := command_field.reg_field.get_csr_regno
          io.debug_core_interface.csr_write_port.write_en := true.B
          io.debug_core_interface.csr_write_port.write_data := w_regdata
          SimLog(
            desiredName,
            "CSR Write addr:%x, wdata:%x, exception:%d\n",
            command_field.reg_field.get_csr_regno,
            w_regdata,
            io.debug_core_interface.csr_write_port.write_ex_resp
          )
        }.otherwise {
          // 读取 csr，读取的时候不在乎 32 位还是 64 位
          io.debug_core_interface.csr_read_port.read_en := true.B
          io.debug_core_interface.csr_read_port.addr := command_field.reg_field.get_csr_regno
          perf_abs_result_buf := io.debug_core_interface.csr_read_port.read_data
          SimLog(
            desiredName,
            "CSR Read addr:%x, rdata:%x, exception:%d\n",
            command_field.reg_field.get_csr_regno,
            io.debug_core_interface.csr_read_port.read_data,
            io.debug_core_interface.csr_read_port.read_ex_resp
          )
        }

        // TODO: csr 不存在怎么办
      }

      perf_abs_state := sPerfABS_REGResp;

    }

    is(sPerfABS_REGResp) {
      when(command_field.reg_field.write === false.B) {
        // 将读取到的寄存器写入到 data_arg 中, 不区分 32 位还是 64 位
        // dmi 读取的时候会自动选择
        arg_write64(0, perf_abs_result_buf)
      }

      // clear busy and cmderr on abstractcs
      set_abstractcs_busy_cmderr(DbgPKG.CMDERR_NONE.U(3.W), false.B)
      perf_abs_state := sPerfABS_IDLE
      // let dmi state machine continue
      dmi_can_exit_side_effect_state := true.B
    }

    is(sPerfABS_MEMReq) {
      val mem_field = command_field.mem_field
      // address on arg64_1
      // wdata on arg64_0
      val abs_mem_size = Mux1H(
        Seq(
          (mem_field.aamsize === DbgPKG.AAMSIZE_8.U) -> AXIDef.SIZE_1,
          (mem_field.aamsize === DbgPKG.AAMSIZE_16.U) -> AXIDef.SIZE_2,
          (mem_field.aamsize === DbgPKG.AAMSIZE_32.U) -> AXIDef.SIZE_4,
          (mem_field.aamsize === DbgPKG.AAMSIZE_64.U) -> AXIDef.SIZE_8
        )
      )(1, 0)
      abs_mem_size_buf := abs_mem_size
      abs_mem_is_write := mem_field.write

      io.debug_core_interface.dcache_req.valid := true.B
      io.debug_core_interface.dcache_req.bits.is_mmio := false.B
      io.debug_core_interface.dcache_req.bits.is_store := mem_field.write
      io.debug_core_interface.dcache_req.bits.paddr := abs_mem_addr
      io.debug_core_interface.dcache_req.bits.wdata := GenAxiWdata(
        abs_mem_wdata,
        abs_mem_addr
      )
      io.debug_core_interface.dcache_req.bits.wstrb := GenAxiWstrb(
        abs_mem_addr,
        abs_mem_size
      )
      io.debug_core_interface.dcache_req.bits.size := abs_mem_size

      // dcache req hs
      when(io.debug_core_interface.dcache_req.fire) {

        when(mem_field.write) {
          SimLog(
            desiredName,
            "Mem Write addr:%x, wdata:%x, wstrb:%x size:%d\n",
            io.debug_core_interface.dcache_req.bits.paddr,
            io.debug_core_interface.dcache_req.bits.wdata,
            io.debug_core_interface.dcache_req.bits.wstrb,
            io.debug_core_interface.dcache_req.bits.size
          )
        }

        perf_abs_state := sPerfABS_MEMResp
      }
    }
    is(sPerfABS_MEMResp) {
      io.debug_core_interface.dcache_resp.ready := true.B
      when(io.debug_core_interface.dcache_resp.fire) {

        when(!abs_mem_is_write) {
          // 将读取到的数据写入到 data_arg 中
          val shifted_rdata =
            GetAxiRdata(
              io.debug_core_interface.dcache_resp.bits.rdata,
              abs_mem_addr,
              abs_mem_size_buf,
              false.B
            )
          arg_write64(0, shifted_rdata)

          SimLog(
            desiredName,
            "Mem Read addr:%x, rdata:%x, size: %d\n",
            abs_mem_addr,
            shifted_rdata,
            abs_mem_size_buf
          )
        }

        set_abstractcs_busy_cmderr(DbgPKG.CMDERR_NONE.U(3.W), false.B)
        perf_abs_state := sPerfABS_IDLE
        // let dmi state machine continue
        dmi_can_exit_side_effect_state := true.B
      }
    }

    is(sPerfABS_ERR) {
      // error
      set_abstractcs_busy_cmderr(cmderr_buf, false.B)
      dmi_can_exit_side_effect_state := true.B
      perf_abs_state := sPerfABS_IDLE
    }
  }

  // -------------------------
  // assert
  // -------------------------
  when(abstractcs_field.busy === false.B) {
    val cmderr_valid_value = VecInit(
      DbgPKG.CMDERR_NONE.U,
      DbgPKG.CMDERR_BUSY.U,
      DbgPKG.CMDERR_NOTSUP.U,
      DbgPKG.CMDERR_EXCEPTION.U,
      DbgPKG.CMDERR_HALT_RESUME.U,
      DbgPKG.CMDERR_BUS.U,
      DbgPKG.CMDERR_RESERVED.U,
      DbgPKG.CMDERR_OTHER.U
    ).contains(abstractcs_field.cmderr)
    assert(
      cmderr_valid_value,
      "This field(cmderr) only contains a valid value if busy is 0."
    )
  }
}

object GenDebugModuleVerilog extends App {
  val dm_config = new DebugModuleConfig()
  GenVerilogHelper(new DebugModule(dm_config))
}
