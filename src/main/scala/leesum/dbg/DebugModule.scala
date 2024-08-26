package leesum.dbg

import chisel3._
import chisel3.util.{DecoupledIO, _}
import leesum.Cache.{DCacheReq, DCacheResp}
import leesum.Utils.DecoderHelper
import leesum._

class DebugModuleConfig {
  val progbuf_num = 4
  val abstract_data_num = 6
  val abits = 6
  val dm_base_addr = 0x0

}

class DMIReq(abits: Int = 6) extends Bundle {
  val addr = UInt(abits.W)
  val data = UInt(32.W)
  val op = UInt(2.W)
}

class DMIResp(abits: Int) extends DMIReq(abits) {}

class DebugModule(dm_config: DebugModuleConfig) extends Module {
  val io = IO(new Bundle {
    //  dm <> core interface
    val debug_state_regs = Input(new DbgSlaveState())
    val debug_halt_req = Output(ValidIO(Bool()))
    val debug_resume_req = Output(ValidIO(Bool()))
    val debug_reset_req = Output(ValidIO(Bool()))
    val debug_clear_havereset = Output(ValidIO(Bool()))
    val debug_gpr_read_port = new RegFileReadPort
    val debug_gpr_write_port = Flipped(new GPRsWritePort)
    val debug_csr_read_port = new CSRReadPort
    val debug_csr_write_port = new CSRWritePort
    val debug_dcache_req = Decoupled(new DCacheReq)
    val debug_dcache_resp = Flipped(Decoupled(new DCacheResp))

    // dmi <> dm interface
    val dmi_req = Flipped(DecoupledIO(new DMIReq(dm_config.abits)))
    val dmi_resp = DecoupledIO(new DMIResp(dm_config.abits))
  })

  def clear_debug_port(): Unit = {
    io.debug_halt_req.valid := false.B
    io.debug_resume_req.valid := false.B
    io.debug_reset_req.valid := false.B
    io.debug_clear_havereset.valid := false.B

    io.debug_halt_req.bits := false.B
    io.debug_resume_req.bits := false.B
    io.debug_reset_req.bits := false.B
    io.debug_clear_havereset.bits := false.B

    io.debug_gpr_read_port.rs1_addr := DontCare
    io.debug_gpr_read_port.rs2_addr := DontCare
    io.debug_gpr_write_port.wen := false.B
    io.debug_gpr_write_port.addr := DontCare
    io.debug_gpr_write_port.wdata := DontCare

    io.debug_csr_read_port.addr := DontCare
    io.debug_csr_read_port.read_en := false.B
    io.debug_csr_write_port.addr := DontCare
    io.debug_csr_write_port.write_data := DontCare
    io.debug_csr_write_port.write_en := false.B

    io.debug_dcache_req.noenq();
    io.debug_dcache_resp.nodeq();
  }

  clear_debug_port();
  // clear_dmi_port
  io.dmi_req.nodeq();
  io.dmi_resp.noenq();

//  // debug module registers
  val progbuf = RegInit(VecInit(Seq.fill(dm_config.progbuf_num)(0.U(32.W))))
  val abstract_data = RegInit(
    VecInit(Seq.fill(dm_config.abstract_data_num)(0.U(32.W)))
  )
  val dmcontrol = RegInit(0.U(32.W))
  val dmstatus = RegInit(0.U(32.W))
  val hartinfo = RegInit(0.U(32.W))
  val abstractcs = RegInit(0.U(32.W))
  val command = RegInit(0.U(32.W))

  def arg_read32(idx: Int): UInt = {
    require(idx < dm_config.abstract_data_num)
    abstract_data(idx)
  }

  def arg_write32(idx: Int, data: UInt): Unit = {
    require(idx < dm_config.abstract_data_num)
    require(data.getWidth == 32)
    abstract_data(idx) := data
  }

  // TODO: 检查排列顺序
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
      printf("dmactive is set to 1, reset debug module\n")
    }

    when(new_dmcontrol_field.ackhavereset) {
      io.debug_clear_havereset.valid := true.B
    }

    when(new_dmcontrol_field.haltreq) {
      // Writing 0 clears the halt request bit for all currently selected harts.
      // This may cancel outstanding halt requests for those harts.
      // Writing 1 sets the halt request bit for all currently
      // selected harts. Running harts will halt whenever
      // their halt request bit is set.
      io.debug_halt_req.valid := true.B
      printf("DM: hart haltreq\"\n")
    }.elsewhen(new_dmcontrol_field.resumereq) {
      // Writing 1 causes the currently selected harts to
      // resume once, if they are halted when the write
      // occurs. It also clears the resume ack bit for those
      // harts.
      // resumereq is ignored if haltreq is set.
      io.debug_resume_req.valid := true.B
      printf("DM: hart resumereq\"\n")
    }

    when(new_dmcontrol_field.hartreset) {
      // This optional field writes the reset bit for all the
      // currently selected harts. To perform a reset the
      // debugger writes 1, and then writes 0 to deassert
      // the reset signal.
      io.debug_reset_req.valid := true.B
    }

    when(new_dmcontrol_field.ndmreset) {
      // This bit controls the reset signal from the DM
      // to the rest of the hardware platform. The signal
      // should reset every part of the hardware platform,
      // including every hart, except for the DM and any
      // logic required to access the DM
      // TODO: only support reset harts
      io.debug_reset_req.valid := true.B
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
      io.debug_state_regs.have_reset()
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
      io.debug_state_regs
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
      (DbgPKG.ABSTRACTAUTO_ADDR, 0.U, empty_read, empty_write)
    )

  for ((addr, reg, read_func, write_func) <- m_map) {
    dm_map.add_csr(addr, reg, read_func, write_func)
  }

  val sDMI_IDLE :: sDMI_PROC :: sDMI_SenResp :: sDMI_SideEff :: Nil = Enum(4)
  val dmi_state = RegInit(sDMI_IDLE)

  val dmi_req_buf = RegInit(0.U.asTypeOf(new DMIReq(dm_config.abits)))
  val dmi_resp_buf = RegInit(0.U.asTypeOf(new DMIResp(dm_config.abits)))
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
          // 1. 写 dmcontrol 寄存器 (控制 hart 状态)
          // 2. 写 command 寄存器 （执行 perform_abstract_command）
          val side_effect_dm_regs = VecInit(
            DbgPKG.DMCONTROL_ADDR.asUInt,
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
              (on_write_side_effect) -> DbgPKG.DMI_OP_STATUS_BUSY.U,
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
      // side effect

    }
  }

  val need_perform_abstract_command = dmi_state === sDMI_SideEff

  dontTouch(need_perform_abstract_command)

  // 须要解析 command 寄存器
  val command_field = new CommandFiled(command)

  // 1. 读取 gpr 寄存器: 不需要握手, 固定周期
  // 2. 读取 csr 寄存器: 需要握手, 不固定周期
  // 3. 读取 dcache: 需要握手, 不固定周期
  val sPerfABS_IDLE :: sPerfABS_REGReq :: sPerfABS_REGResp :: sPerfABS_MEMReq :: sPerfABS_MEMResp :: sPerfABS_ERR :: Nil =
    Enum(6)

  val perf_abs_state = RegInit(sPerfABS_IDLE)
  val perf_abs_result_buf = RegInit(0.U(64.W))

//  dontTouch(perf_abs_state)

  switch(perf_abs_state) {
    is(sPerfABS_IDLE) {
      when(need_perform_abstract_command) {

//        perf_abs_state := DecoderHelper.gen(
//          command_field.cmdtype,
//          sPerfABS_ERR,
//          Seq(
//            DbgPKG.COMDTYPE_ACCESS_REG.U(8.W) -> sPerfABS_REG,
//            DbgPKG.COMDTYPE_ACCESS_MEM.U(8.W) -> sPerfABS_MEM
//          )
//        )

        perf_abs_state := MuxLookup(
          command_field.cmdtype,
          sPerfABS_ERR
        ) {
          Seq(
            DbgPKG.COMDTYPE_ACCESS_REG.U -> sPerfABS_REGReq,
            DbgPKG.COMDTYPE_ACCESS_MEM.U -> sPerfABS_MEMReq
          )
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
        (reg_field.is_gpr && reg_field.is_csr) === false.B,
        "only either gpr or csr"
      )

      // 读写 gpr
      when(command_field.reg_field.is_gpr && !cmd_not_support) {
        when(command_field.reg_field.write) {
          // 写 gpr
          io.debug_gpr_write_port.wen := true.B
          io.debug_gpr_write_port.addr := command_field.reg_field.get_gpr_regno
          io.debug_gpr_write_port.wdata := w_regdata
        }.otherwise {
          io.debug_gpr_read_port.rs1_addr := command_field.reg_field.get_gpr_regno
          perf_abs_result_buf := io.debug_gpr_read_port.rs1_data
        }

      }
      // 读写 csr
      when(command_field.reg_field.is_csr && !cmd_not_support) {
        when(command_field.reg_field.write) {
          // 写 csr
          io.debug_csr_write_port.addr := command_field.reg_field.get_csr_regno
          io.debug_csr_write_port.write_en := true.B
          io.debug_csr_write_port.write_data := w_regdata
        }.otherwise {
          // 读取 csr
          io.debug_csr_read_port.read_en := true.B
          io.debug_csr_read_port.addr := command_field.reg_field.get_csr_regno
          perf_abs_result_buf := io.debug_csr_read_port.read_data
        }
        // TODO: csr 不存在怎么办
      }

      perf_abs_state := Mux(
        cmd_not_support,
        sPerfABS_ERR,
        sPerfABS_REGResp
      )
    }

    is(sPerfABS_REGResp) {
      when(command_field.reg_field.write === false.B) {
        // 将读取到的寄存器写入到 data_arg 中, 不区分 32 位还是 64 位
        arg_write64(0, perf_abs_result_buf)
      }

      perf_abs_state := sPerfABS_IDLE
      // 让 dmi 状态机继续
      dmi_state := sDMI_SenResp
    }

    is(sPerfABS_MEMReq) {
      // 读取 mem
      perf_abs_state := sPerfABS_IDLE
    }

    is(sPerfABS_MEMResp) {
      // 读取 mem
      perf_abs_state := sPerfABS_IDLE
    }

    is(sPerfABS_ERR) {
      // error
      perf_abs_state := sPerfABS_IDLE
    }
  }
}

object GenDebugModuleVerilog extends App {
  val dm_config = new DebugModuleConfig()

  GenVerilogHelper(new DebugModule(dm_config))
}
