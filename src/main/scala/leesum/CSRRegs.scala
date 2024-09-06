package leesum
import chisel3._
import chisel3.util.{Cat, Mux1H, MuxLookup, Valid}

class CSRMap {
  type ReadFunc = (UInt, UInt) => Valid[UInt]
  type WriteFunc = (UInt, UInt, UInt) => Valid[UInt]

  val csr_map = collection.mutable
    .Map[
      Int, // csr_addr
      (UInt, ReadFunc, WriteFunc) // reg,read_func,write_func
    ]()

  def add_csr(
      addr: Int,
      reg: UInt,
      read_func: ReadFunc,
      write_func: WriteFunc
  ) = {

    require(addr >= 0 && addr < 4096, s"csr addr $addr should be in [0, 4096)")
    require(!csr_map.contains(addr), s"csr addr $addr already exists")

//    if (csr_map.isEmpty) {
    csr_map.addOne(addr, (reg, read_func, write_func))
//    }

  }

  /** This function is used to read csr register, if success, return a valid
    * UInt, otherwise return a invalid UInt
    * @param raddr
    *   csr address
    * @return
    *   Valid(UInt): bits is the read result
    */
  def read(raddr: UInt, use_one_hot: Boolean = true): Valid[UInt] = {

    val default_witdth = csr_map.head._2._1.getWidth
    val default_read = Wire(Valid(UInt(default_witdth.W)))
    default_read.valid := false.B
    default_read.bits := 0.U

    val raddr_map = csr_map
      .map({ case (addr, (reg, read_func, _)) =>
        val read_result = read_func(addr.U, reg)
        (addr.U, read_result)
      })
      .toSeq

    val raddr_map_1h = csr_map
      .map({ case (addr, (reg, read_func, _)) =>
        val read_result = read_func(addr.U, reg)
        (addr.U === raddr, read_result)
      })
      .toSeq
    // at least one csr read result is valid
    val oneH_valid = raddr_map_1h.map(_._1).reduce(_ || _)
    val rdata_1h = Mux(oneH_valid, Mux1H(raddr_map_1h), default_read)

    val rdata = MuxLookup(raddr, default_read)(raddr_map)
    if (use_one_hot) rdata_1h else rdata
  }

  /** This function is used to write csr register, if success, return a valid
    * UInt, otherwise return a invalid UInt
    * @param waddr
    *   csr address
    * @param wdata
    *   write data
    * @return
    *   Valid(UInt): bits is the write result
    */
  def write(waddr: UInt, wdata: UInt): Valid[UInt] = {
    val write_result = Wire(Valid(UInt(64.W)))
    write_result.valid := false.B
    write_result.bits := 0.U
    csr_map.foreach({ case (addr, (reg, _, write_func)) =>
      when(waddr === addr.U) {
        write_result := write_func(addr.U, reg, wdata)
      }
    })
    write_result
  }
}
//
//object CSRMap {
//
//  def empty_write_func(reg_width: Int) = {
//    val empty_write = (addr: UInt, reg: UInt, wdata: UInt) => {
//      val write_result = Wire(Valid(UInt(reg_width.W)))
//      write_result.valid := true.B
//      write_result.bits := 0.U
//      write_result
//    }
//
//    empty_write
//  }
//  def zero_read_func(reg_width: Int) = {
//    val empty_read = (addr: UInt, reg: UInt) => {
//      val read_result = Wire(Valid(UInt(reg_width.W)))
//      read_result.valid := true.B
//      read_result.bits := 0.U
//      read_result
//    }
//    empty_read
//  }
//  def normal_read_func(reg_width: Int) = {
//    val normal_read = (addr: UInt, reg: UInt) => {
//      val read_result = Wire(Valid(UInt(reg_width.W)))
//      read_result.valid := true.B
//      read_result.bits := reg
//      read_result
//    }
//    normal_read
//  }
//  def normal_write_func(reg_width: Int) = {
//    val normal_write = (addr: UInt, reg: UInt, wdata: UInt) => {
//      val write_result = Wire(Valid(UInt(reg_width.W)))
//      write_result.valid := true.B
//      write_result.bits := wdata
//      reg := write_result.bits
//      write_result
//    }
//    normal_write
//  }
//}

class CSRReadPort extends Bundle {
  val addr = Output(UInt(12.W))
  val read_en = Output(Bool())
  val read_data = Input(UInt(64.W))
  val read_ex_resp = Input(Bool())
}
class CSRWritePort extends Bundle {
  val addr = Output(UInt(12.W))
  val write_en = Output(Bool())
  val write_data = Output(UInt(64.W))
  // if write_ex_resp is true, the write operation is not successful
  val write_ex_resp = Input(Bool())
}

class CSRDirectReadPorts extends Bundle {
  val mstatus = UInt(64.W)
  val mie = UInt(64.W)
  val mip = UInt(64.W)
  val misa = UInt(64.W)
  // m mode
  val mcause = UInt(64.W)
  val mtvec = UInt(64.W)
  val mepc = UInt(64.W)
  val mtval = UInt(64.W)
  val mscratch = UInt(64.W)
  val medeleg = UInt(64.W)
  val mideleg = UInt(64.W)
  // s mode
  val scause = UInt(64.W)
  val stvec = UInt(64.W)
  val sepc = UInt(64.W)
  val stval = UInt(64.W)
  val sscratch = UInt(64.W)

  // Debug mode
  val dcsr = UInt(32.W)
  val dpc = UInt(64.W)

  val satp = UInt(64.W)
}

class DcsrFiled(data: UInt) {
  require(data.getWidth == 32)
  def prv = data(1, 0)
  def step = data(2)
  def nmip = data(3)
  def mprven = data(4)
  def v = data(5)
  def cause = data(8, 6)
  def stoptime = data(9)
  def stopcount = data(10)
  def stepie = data(11)
  def ebreaku = data(12)
  def ebreaks = data(13)
  def zero0 = data(14)
  def ebreakm = data(15)
  def ebreakvu = data(16)
  def ebreakvs = data(17)
  def zero1 = data(27, 18)
  def debugver = data(31, 28)

  def get_debug_dcsr(cause: UInt, cur_priv: UInt) = {
    require(cause.getWidth == 3)
    require(cur_priv.getWidth == 2)
    val new_dcsr = Cat(
      data(31, 9),
      cause,
      false.B, // not support v
      data(4, 2),
      cur_priv
    )
    require(new_dcsr.getWidth == 32)
    new_dcsr
  }

}

class MstatusFiled(data: UInt) {
  require(data.getWidth == 64)

  def sie = data(1)
  def mie = data(3)

  def spie = data(5)
  def mpie = data(7)
  def spp = data(8)
  def vs = data(10, 9)
  def mpp = data(12, 11)
  def fs = data(14, 13)
  def xs = data(16, 15)
  def mprv = data(17)
  def sum = data(18)
  def mxr = data(19)
  def tvm = data(20)
  def tw = data(21)
  def tsr = data(22)
  def uxl = data(33, 32)
  def sxl = data(35, 34)
  def sbe = data(36)
  def mbe = data(37)
  def sb = data(63)

  def get_mmode_exception_mstatus(cur_privilege: UInt) = {
    require(cur_privilege.getWidth == 2)

    val new_cause = Cat(
      data(63, 13),
      cur_privilege, // mpp
      data(10, 8),
      this.mie, // mpie
      data(6, 4),
      false.B, // mie
      data(2, 0)
    )
    require(new_cause.getWidth == 64)
    new_cause
  }

  def get_smode_exception_mstatus(cur_privilege: UInt) = {
    require(cur_privilege.getWidth == 2)

    val new_cause = Cat(
      data(63, 9),
      // When a trap is taken, SPP is set to 0 if the trap originated from user mode, or 1 otherwise.
      Mux(cur_privilege === Privilegelevel.U.U, false.B, true.B), // spp
      data(7, 6),
      // When a trap is taken into supervisor mode, SPIE is set to SIE
      this.sie, // spie
      data(4, 2),
      // and SIE is set to 0
      false.B, // sie
      data(0)
    )
    require(new_cause.getWidth == 64)
    new_cause
  }

  def get_mret_mstatus(clear_mprv: Bool) = {
    val new_mstatus = Cat(
      data(63, 18),
      Mux(clear_mprv, false.B, this.mprv), // mprv
      data(16, 13),
      // mpp , xPP is set to the least-privileged supported mode (U if U-mode is implemented, else M).
      Privilegelevel.U.U(2.W),
      data(10, 8),
      true.B, // mpie // xPIE is set to 1;
      data(6, 4),
      this.mpie, // mie // xIE is set to xPIE
      data(2, 0)
    )

    require(new_mstatus.getWidth == 64)
    new_mstatus
  }
  def get_sret_mstatus(clear_mprv: Bool) = {
    val new_mstatus = Cat(
      data(63, 18),
      Mux(clear_mprv, false.B, this.mprv), // mprv
      data(16, 9),
      // spp , xPP is set to the least-privileged supported mode (U if U-mode is implemented, else M).
      false.B,
      data(7, 6),
      true.B, // spie // xPIE is set to 1;
      data(4, 2),
      this.spie, // sie // xIE is set to xPIE
      data(0)
    )
    require(new_mstatus.getWidth == 64)
    new_mstatus
  }

  def get_exit_debug_mstatus(cur_priv: UInt) = {
    require(cur_priv.getWidth == 2)
    val new_mstatus = Cat(
      data(63, 18),
      Mux(cur_priv < Privilegelevel.M.U, false.B, this.mprv), // mpr
      data(16, 0)
    )
    require(new_mstatus.getWidth == 64)
    new_mstatus
  }

}

class MtvecFiled(data: UInt) {
  require(data.getWidth == 64)
  def base: UInt = data(63, 2)
  def mode: UInt = data(1, 0)

  def get_exception_pc: UInt = {
    val base = Cat(this.base, 0.U(2.W))
    require(base.getWidth == 64)
    base
  }

  def get_interrupt_pc(cause: UInt): UInt = {
    val mode = this.mode
    val base = Cat(this.base, 0.U(2.W))

    val pc = Wire(UInt(64.W))
    assert(mode < 2.U, "mode should be 0 or 1")
    pc := 0.U
    when(mode === 0.U) {
      pc := base
    }.otherwise {
      // TODO: dn not use shift
      pc := base + (cause << 2.U)
    }
    pc
  }

}

class McauseFiled(data: UInt) {
  require(data.getWidth == 64)
  def code: UInt = data(62, 0)
  def interrupt: Bool = data(63)
}

class MieFiled(data: UInt) {
  require(data.getWidth == 64)
  def ssie: Bool = data(1)
  def msie: Bool = data(3)
  def stie: Bool = data(5)
  def mtie: Bool = data(7)
  def seie: Bool = data(9)
  def meie: Bool = data(11)

  def raw: UInt = data
}

class MipFiled(data: UInt) {
  require(data.getWidth == 64)
  def ssip: Bool = data(1)
  def msip: Bool = data(3)
  def stip: Bool = data(5)
  def mtip: Bool = data(7)
  def seip: Bool = data(9)
  def meip: Bool = data(11)

  def any_interrupt: Bool = {
    ssip || msip || stip || mtip || seip || meip
  }

  def get_priority_interupt: ExceptionCause.Type = {
    val priority_int = WireInit(ExceptionCause.unknown)
    when(meip) {
      priority_int := ExceptionCause.machine_external_interrupt
    }.elsewhen(msip) {
      priority_int := ExceptionCause.machine_software_interrupt
    }.elsewhen(mtip) {
      priority_int := ExceptionCause.machine_timer_interrupt
    }.elsewhen(seip) {
      priority_int := ExceptionCause.supervisor_external_interrupt
    }.elsewhen(ssip) {
      priority_int := ExceptionCause.supervisor_software_interrupt
    }.elsewhen(stip) {
      priority_int := ExceptionCause.supervisor_timer_interrupt
    }
    priority_int
  }

  def raw = data
}

class SatpFiled(data: UInt) {
  require(data.getWidth == 64)
  def mode: UInt = data(63, 60)
  def asid: UInt = data(59, 44)
  def ppn: UInt = data(43, 0)

  def mode_is_sv39: Bool = mode === 8.U
  def mode_is_bare: Bool = mode === 0.U

  def mode_is_unsupported: Bool = !mode_is_sv39 && !mode_is_bare
}

class MidelegFiled(data: UInt) extends MipFiled(data)

class CSRDirectWritePorts extends Bundle {
  val mstatus = Valid(UInt(64.W))
  val mie = Valid(UInt(64.W))
  val mip = Valid(UInt(64.W))

  val mcause = Valid(UInt(64.W))
  val mtvec = Valid(UInt(64.W))
  val mepc = Valid(UInt(64.W))
  val mtval = Valid(UInt(64.W))

  val scause = Valid(UInt(64.W))
  val stvec = Valid(UInt(64.W))
  val sepc = Valid(UInt(64.W))
  val stval = Valid(UInt(64.W))

  // Debug mode

  val dcsr = Valid(UInt(32.W))
  val dpc = Valid(UInt(64.W))

  val instret_inc = Valid(UInt(4.W))

  def any_valid(): Bool = {
    Seq(
      mstatus.valid,
      mie.valid,
      mip.valid,
      mcause.valid,
      mtvec.valid,
      mepc.valid,
      mtval.valid,
      sepc.valid,
      stval.valid,
      stvec.valid,
      scause.valid,
      instret_inc.valid,
      dcsr.valid,
      dpc.valid
    ).reduce(_ || _)
  }

  def clear(): Unit = {
    mstatus.valid := false.B
    mcause.valid := false.B
    mie.valid := false.B
    mip.valid := false.B
    mtvec.valid := false.B
    mepc.valid := false.B
    mtval.valid := false.B
    sepc.valid := false.B
    stval.valid := false.B
    stvec.valid := false.B
    scause.valid := false.B
    instret_inc.valid := false.B
    dcsr.valid := false.B
    dpc.valid := false.B

    mstatus.bits := 0.U
    mcause.bits := 0.U
    mie.bits := 0.U
    mip.bits := 0.U
    mtvec.bits := 0.U
    mepc.bits := 0.U
    mtval.bits := 0.U
    sepc.bits := 0.U
    stval.bits := 0.U
    stvec.bits := 0.U
    scause.bits := 0.U
    instret_inc.bits := 0.U
    dcsr.bits := 0.U
    dpc.bits := 0.U
  }
}

class CSRRegs(read_port_num: Int = 1, write_port_num: Int = 1) extends Module {
  val io = IO(new Bundle {
    val read_ports = Vec(read_port_num, Flipped(new CSRReadPort))
    val write_ports = Vec(write_port_num, Flipped(new CSRWritePort))
    val direct_read_ports = Output(new CSRDirectReadPorts)
    val direct_write_ports = Input(new CSRDirectWritePorts)
    // privilege mode
    val cur_privilege_mode = Input(UInt(2.W))

    val mtime = Input(UInt(64.W))
    val time_int = Input(Bool())
    val soft_int = Input(Bool())
    val mext_int = Input(Bool())
    val sext_int = Input(Bool())
  })

  // TODO: use config replace hard code
  val smode_enable = true
  val umode_enable = true
  val float_enable = false

  val csr_map = new CSRMap()

  // -----------------------
  // mstatus init value
  // -----------------------
  val mstatus_val = new CSRBitField(0)

  mstatus_val.set_field(MStatusMask.MPP, 3) // machine mode
  mstatus_val.set_field(MStatusMask.MBE, 0)
  mstatus_val.set_field(MStatusMask.SBE, 0)
  mstatus_val.set_field(MStatusMask.UBE, 0)

  if (smode_enable) {
    mstatus_val.set_field(MStatusMask.SXL, 2) // xlen = 64
  }
  if (umode_enable) {
    mstatus_val.set_field(MStatusMask.UXL, 2) // xlen = 64
    mstatus_val.set_field(MStatusMask.MPRV, 0)
  }

  println("mstatus_val: " + mstatus_val.get_raw.toHexString)

  // ---------------------
  // mstatus read func
  // ---------------------
  val mstatus_rmask = new CSRBitField(-1L)
  // only support little endian
  mstatus_rmask.set_field(MStatusMask.SBE, 0)
  mstatus_rmask.set_field(MStatusMask.MBE, 0)
  mstatus_rmask.set_field(MStatusMask.UBE, 0)

  if (!smode_enable) {
    mstatus_rmask.set_field(MStatusMask.MXR, 0)
    mstatus_rmask.set_field(MStatusMask.SUM, 0)
    mstatus_rmask.set_field(MStatusMask.TVM, 0)
    mstatus_rmask.set_field(MStatusMask.TSR, 0)
  }

  if (!umode_enable) {
    mstatus_rmask.set_field(MStatusMask.MPRV, 0)
  }

  if (!umode_enable && !smode_enable) {
    mstatus_rmask.set_field(MStatusMask.TW, 0)
  }

  if (!float_enable) {
    mstatus_rmask.set_field(MStatusMask.FS, 0)
    mstatus_rmask.set_field(MStatusMask.VS, 0)
    mstatus_rmask.set_field(MStatusMask.XS, 0)
    mstatus_rmask.set_field(MStatusMask.MSTATUS64_SD, 0)
    println("float_enable: " + float_enable)
  }

  println("mstatus_rmask: " + mstatus_rmask.get_raw.toHexString)

  val mstatus_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(64.W)))
    read_result.valid := true.B
    read_result.bits := reg & Long2UInt64(mstatus_rmask.get_raw)
    read_result
  }

  // --------------------
  // mstatus write fun
  // --------------------
  val mstatus_wmask = new CSRBitField(mstatus_rmask.get_raw)
  mstatus_wmask.set_field(MStatusMask.UXL, 0)
  mstatus_wmask.set_field(MStatusMask.SXL, 0)

  val mstatus_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(64.W)))
    write_result.valid := true.B
    write_result.bits := wdata & Long2UInt64(
      mstatus_wmask.get_raw
    ) | reg & Long2UInt64(~mstatus_wmask.get_raw)
    reg := write_result.bits
    write_result
  }

  println("mstatus_wmask: " + mstatus_wmask.get_raw.toHexString)

  // ---------------------
  // sstatus read func
  // ---------------------
  val sstatus_read = mstatus_read

  // ---------------------
  // sstatus write func
  // ---------------------
  val sstatus_wmask_before = new CSRBitField(0L)
  sstatus_wmask_before.set_field(MStatusMask.SPP, 1)
  sstatus_wmask_before.set_field(MStatusMask.SIE, 1)
  sstatus_wmask_before.set_field(MStatusMask.SPIE, 1)
  sstatus_wmask_before.set_field(MStatusMask.UBE, 1)
  sstatus_wmask_before.set_field(MStatusMask.VS, 3)
  sstatus_wmask_before.set_field(MStatusMask.FS, 3)
  sstatus_wmask_before.set_field(MStatusMask.XS, 3)
  sstatus_wmask_before.set_field(MStatusMask.SUM, 1)
  sstatus_wmask_before.set_field(MStatusMask.MXR, 1)
  sstatus_wmask_before.set_field(MStatusMask.MSTATUS64_SD, 1)
  sstatus_wmask_before.get_raw

  val sstatus_wmask =
    sstatus_wmask_before.get_raw & mstatus_rmask.get_raw

  val sstatus_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(64.W)))
    write_result.valid := true.B
    write_result.bits := wdata & Long2UInt64(
      sstatus_wmask
    ) | reg & Long2UInt64(~sstatus_wmask)
    reg := write_result.bits
    write_result
  }

  // -------------------------
  // sip & sie read write func
  // -------------------------
  val sip_sie_mask = new CSRBitField(0)
  sip_sie_mask.set_field(MipMask.seip, 1)
  sip_sie_mask.set_field(MipMask.ssip, 1)
  sip_sie_mask.set_field(MipMask.stip, 1)
  val sip_sie_mask_raw = Long2UInt64(sip_sie_mask.get_raw)

  val sip_sie_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(64.W)))
    read_result.valid := true.B
    read_result.bits := reg & sip_sie_mask_raw
    read_result
  }

  val sip_sie_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(64.W)))
    write_result.valid := true.B
    write_result.bits := wdata & sip_sie_mask_raw | reg & (~sip_sie_mask_raw).asUInt
    reg := write_result.bits
    write_result
  }

  // ---------------------
  // satp write func
  // ---------------------

  def satp_permission_ok(cur_privilege: UInt) = {
    require(cur_privilege.getWidth == 2)
    val tvm = new MstatusFiled(mstatus).tvm
    val require_privilege = Mux(tvm, Privilegelevel.M.U, Privilegelevel.S.U)
    cur_privilege >= require_privilege
  }

  val satp_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(64.W)))
    when(satp_permission_ok(io.cur_privilege_mode)) {
      val satp_val = new SatpFiled(wdata)
      // WAWL
      val effective_mode =
        Mux(
          satp_val.mode_is_sv39 || satp_val.mode_is_bare,
          satp_val.mode,
          reg(63, 60)
        )
      write_result.valid := true.B
      write_result.bits := Cat(effective_mode, satp_val.asid, satp_val.ppn)
      reg := write_result.bits
    }.otherwise {
      write_result.valid := false.B
      write_result.bits := 0.U
    }
    write_result
  }

  // ---------------------
  // satp read func
  // ---------------------

  val satp_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(64.W)))
    when(satp_permission_ok(io.cur_privilege_mode)) {
      read_result.valid := true.B
      read_result.bits := reg
    }.otherwise {
      read_result.valid := false.B
      read_result.bits := 0.U
    }
    read_result
  }

  // -----------------
  // misa register
  // -----------------
  val misa_val = new CSRBitField(0)
  misa_val.set_field(MIsaMask.I, 1)
  misa_val.set_field(MIsaMask.M, 1)
  misa_val.set_field(MIsaMask.A, 1)
  misa_val.set_field(MIsaMask.C, 1)

  if (umode_enable) {
    misa_val.set_field(MIsaMask.U, 1)
  }
  if (smode_enable) {
    misa_val.set_field(MIsaMask.S, 1)
  }
  misa_val.set_field(MIsaMask.MXL, 2) // xlen = 64

  // -----------------
  // csr register
  // -----------------

  println("mstatus_val: " + mstatus_val.get_raw.toHexString)
  val mstatus = RegInit(Long2UInt64(mstatus_val.get_raw))
  val mie = RegInit(0.U(64.W))
  val mip = RegInit(0.U(64.W))
  val mcause = RegInit(0.U(64.W))
  val mtvec = RegInit(0.U(64.W))
  val mepc = RegInit(0.U(64.W))
  val mtval = RegInit(0.U(64.W))
  val mscratch = RegInit(0.U(64.W))
  // TODO: make misa configurable
  val misa = RegInit(Long2UInt64(misa_val.get_raw))
  val mimpid = RegInit(0.U(64.W))
  val mhartid = RegInit(0.U(64.W))
  val marchid = RegInit(0.U(64.W))
  val mvendorid = RegInit(0.U(64.W))

  // smode
  val scause = RegInit(0.U(64.W))
  val stvec = RegInit(0.U(64.W))
  val sepc = RegInit(0.U(64.W))
  val stval = RegInit(0.U(64.W))
  val sscratch = RegInit(0.U(64.W))
  val satp = RegInit(0.U(64.W))

  // Debug mode

  val dcsr = RegInit(0.U(64.W))
  val dpc = RegInit(0.U(64.W))

  // counters
  val mcounteren = RegInit(0.U(64.W))
  val scounteren = RegInit(0.U(64.W))

  val cycle = RegInit(0.U(64.W))
  val instret = RegInit(0.U(64.W))
  cycle := cycle + 1.U
  when(io.direct_write_ports.instret_inc.valid) {
    instret := instret + io.direct_write_ports.instret_inc.bits
  }

  val mideleg = RegInit(0.U(64.W))
  val medeleg = RegInit(0.U(64.W))

  // not support debug mode,just to pass breakpoint test
  // Skip tselect if hard-wired. RISC-V Debug Specification
  val tselect = RegInit(GenMaskOne(64, 64))

  // -------------------------
  // read write func define
  // -------------------------
  val normal_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(64.W)))
    read_result.valid := true.B
    read_result.bits := reg
    read_result
  }
  val normal_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(64.W)))
    write_result.valid := true.B
    write_result.bits := wdata
    reg := write_result.bits
    write_result
  }

  val empty_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(64.W)))
    write_result.valid := true.B
    write_result.bits := 0.U
    write_result
  }

  // TODO: warl field
  val m_map =
    Seq(
      (CSRs.mstatus, mstatus, mstatus_read, mstatus_write),
      (CSRs.mcause, mcause, normal_read, normal_write),
      (CSRs.mie, mie, normal_read, normal_write),
      (CSRs.mtvec, mtvec, normal_read, normal_write),
      (CSRs.mtval, mtval, normal_read, normal_write),
      (CSRs.mepc, mepc, normal_read, normal_write),
      (CSRs.mip, mip, normal_read, normal_write),
      (CSRs.mscratch, mscratch, normal_read, normal_write),
      (CSRs.mideleg, mideleg, normal_read, normal_write),
      (CSRs.medeleg, medeleg, normal_read, normal_write),
      // smode
      (CSRs.sstatus, mstatus, sstatus_read, sstatus_write),
      (CSRs.scause, scause, normal_read, normal_write),
      (CSRs.stvec, stvec, normal_read, normal_write),
      (CSRs.stval, stval, normal_read, normal_write),
      (CSRs.sepc, sepc, normal_read, normal_write),
      (CSRs.sscratch, sscratch, normal_read, normal_write),
      (CSRs.satp, satp, satp_read, satp_write),
      (CSRs.sip, mip, sip_sie_read, sip_sie_write),
      (CSRs.sie, mie, sip_sie_read, sip_sie_write),

      // Debug mode
      (CSRs.dcsr, dcsr, normal_read, normal_write),
      (CSRs.dpc, dpc, normal_read, normal_write),

      // read only
      (CSRs.misa, misa, normal_read, empty_write),
      (CSRs.mhartid, mhartid, normal_read, empty_write),
      (CSRs.mimpid, mimpid, normal_read, empty_write),
      (CSRs.marchid, marchid, normal_read, empty_write),
      (CSRs.mvendorid, mvendorid, normal_read, empty_write),
      // counters TODO: not support now
      (CSRs.mcounteren, mcounteren, normal_read, normal_write),
      (CSRs.scounteren, scounteren, normal_read, normal_write),
      (CSRs.cycle, cycle, normal_read, empty_write),
      (CSRs.mcycle, cycle, normal_read, empty_write),
      (CSRs.time, RegNext(io.mtime), normal_read, empty_write),
      (CSRs.instret, instret, normal_read, empty_write),
      (CSRs.minstret, instret, normal_read, empty_write),
      (CSRs.tselect, tselect, normal_read, empty_write)
    )
  m_map.foreach({ case (addr, reg, read_func, write_func) =>
    csr_map.add_csr(addr, reg, read_func, write_func)
  })

  // -----------------------
  // read write logic
  // -----------------------
  // io.write_port.write_ex_resp := false.B
  // io.read_port.read_ex_resp := false.B
  // io.read_port.read_data := 0.U

  io.write_ports.foreach(_.write_ex_resp := false.B)
  io.read_ports.foreach(_.read_ex_resp := false.B)
  io.read_ports.foreach(_.read_data := 0.U)

  def check_csr_permission(addr: UInt, privilege_mode: UInt, is_write: Bool) = {
    assert(addr < 4096.U, "csr addr should be in [0, 4096)")
    val csr_privilege_mode = addr(9, 8)
    val csr_privilege_mode_ok = privilege_mode >= csr_privilege_mode
    val csr_read_only = addr(11, 10) === 3.U
    val csr_ok = csr_privilege_mode_ok && Mux(is_write, !csr_read_only, true.B)
    csr_ok
  }

  for (write_port <- io.write_ports) {
    when(write_port.write_en) {
      when(
        check_csr_permission(write_port.addr, io.cur_privilege_mode, true.B)
      ) {
        val w_ret = csr_map.write(write_port.addr, write_port.write_data)
        write_port.write_ex_resp := !w_ret.valid
      }.otherwise {
        write_port.write_ex_resp := true.B
      }
    }
  }

  // 不能同时写入
  assert(
    !io.write_ports.map(_.write_en).reduce(_ && _),
    "can not write csr at the same time"
  )

  for (read_port <- io.read_ports) {
    when(read_port.read_en) {
      when(
        check_csr_permission(read_port.addr, io.cur_privilege_mode, false.B)
      ) {
        val r_ret = csr_map.read(read_port.addr)
        read_port.read_data := r_ret.bits
        read_port.read_ex_resp := !r_ret.valid
      }.otherwise {
        read_port.read_ex_resp := true.B
      }
    }
  }

  // when(io.write_port.write_en) {
  //   when(
  //     check_csr_permission(io.write_port.addr, io.cur_privilege_mode, true.B)
  //   ) {
  //     val w_ret = csr_map.write(io.write_port.addr, io.write_port.write_data)
  //     io.write_port.write_ex_resp := !w_ret.valid
  //   }.otherwise {
  //     io.write_port.write_ex_resp := true.B
  //   }
  // }
  // when(io.read_port.read_en) {
  //   when(
  //     check_csr_permission(io.read_port.addr, io.cur_privilege_mode, false.B)
  //   ) {
  //     val r_ret = csr_map.read(io.read_port.addr)
  //     io.read_port.read_data := r_ret.bits
  //     io.read_port.read_ex_resp := !r_ret.valid
  //   }.otherwise {
  //     io.read_port.read_ex_resp := true.B
  //   }
  // }

  // -----------------------
  // direct read write logic
  // -----------------------

  // m mode
  io.direct_read_ports.mstatus := mstatus
  io.direct_read_ports.mcause := mcause
  io.direct_read_ports.mie := mie

  // TODO: improve this
  io.direct_read_ports.mip := mip
    .bitSet(ExceptionCause.machine_timer_interrupt.asUInt(3, 0), io.time_int)
    .bitSet(ExceptionCause.machine_software_interrupt.asUInt(3, 0), io.soft_int)
    .bitSet(ExceptionCause.machine_external_interrupt.asUInt(3, 0), io.mext_int)
    .bitSet(
      ExceptionCause.supervisor_external_interrupt.asUInt(3, 0),
      io.sext_int
    )
  io.direct_read_ports.mtvec := mtvec
  io.direct_read_ports.mepc := mepc
  io.direct_read_ports.mtval := mtval
  io.direct_read_ports.medeleg := medeleg
  io.direct_read_ports.mideleg := mideleg
  io.direct_read_ports.mscratch := mscratch
  io.direct_read_ports.misa := misa

  // s mode
  io.direct_read_ports.scause := scause
  io.direct_read_ports.stvec := stvec
  io.direct_read_ports.sepc := sepc
  io.direct_read_ports.stval := stval
  io.direct_read_ports.satp := satp
  io.direct_read_ports.sscratch := sscratch

  // debug mode
  io.direct_read_ports.dcsr := dcsr(31, 0)
  io.direct_read_ports.dpc := dpc

  when(io.direct_write_ports.mip.valid) {
    mip := io.direct_write_ports.mip.bits
  }
  when(io.direct_write_ports.mie.valid) {
    mie := io.direct_write_ports.mie.bits
  }
  when(io.direct_write_ports.mstatus.valid) {
    mstatus := io.direct_write_ports.mstatus.bits
  }
  // m mode trap
  when(io.direct_write_ports.mepc.valid) {
    mepc := io.direct_write_ports.mepc.bits
  }
  when(io.direct_write_ports.mtval.valid) {
    mtval := io.direct_write_ports.mtval.bits
  }
  when(io.direct_write_ports.mtvec.valid) {
    mtvec := io.direct_write_ports.mtvec.bits
  }
  when(io.direct_write_ports.mcause.valid) {
    mcause := io.direct_write_ports.mcause.bits
  }
  // s mode trap
  when(io.direct_write_ports.sepc.valid) {
    sepc := io.direct_write_ports.sepc.bits
  }
  when(io.direct_write_ports.stval.valid) {
    stval := io.direct_write_ports.stval.bits
  }
  when(io.direct_write_ports.stvec.valid) {
    stvec := io.direct_write_ports.stvec.bits
  }
  when(io.direct_write_ports.scause.valid) {
    scause := io.direct_write_ports.scause.bits
  }

  // debug mode
  when(io.direct_write_ports.dcsr.valid) {
    dcsr := Cat(0.U(32.W), io.direct_write_ports.dcsr.bits)
  }
  when(io.direct_write_ports.dpc.valid) {
    dpc := io.direct_write_ports.dpc.bits
  }

}

object gen_csr_regs_verilog extends App {
  GenVerilogHelper(new CSRRegs)
}
