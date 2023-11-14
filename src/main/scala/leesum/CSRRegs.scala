package leesum
import chisel3.util.{Cat, MuxLookup, Valid}
import chisel3.{Reg, _}

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

    csr_map.addOne(addr, (reg, read_func, write_func))
  }

  /** This function is used to read csr register, if success, return a valid
    * UInt, otherwise return a invalid UInt
    * @param raddr
    *   csr address
    * @return
    *   Valid(UInt): bits is the read result
    */
  def read(raddr: UInt): Valid[UInt] = {
    val raddr_map = csr_map.map({ case (addr, (reg, read_func, _)) =>
      val read_result = read_func(addr.U, reg)
      (addr.U, read_result)
    })

    val default = Wire(Valid(UInt(64.W)))
    default.valid := false.B
    default.bits := 0.U

    val rdata = MuxLookup(raddr, default)(
      raddr_map.toSeq
    )
    rdata
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
  val mcause = UInt(64.W)
  val mie = UInt(64.W)
  val mip = UInt(64.W)
  val mtvec = UInt(64.W)
  val mepc = UInt(64.W)
  val mtval = UInt(64.W)
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

  def get_exception_mstatus(cur_privilege: UInt) = {
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
}

class MtvecFiled(data: UInt) {
  require(data.getWidth == 64)
  def base: UInt = data(63, 2)
  def mode: UInt = data(1, 0)

  def get_exception_pc(cause: UInt): UInt = {
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
}

class MipFiled(data: UInt) {
  require(data.getWidth == 64)
  def ssip: Bool = data(1)
  def msip: Bool = data(3)
  def stip: Bool = data(5)
  def mtip: Bool = data(7)
  def seip: Bool = data(9)
  def meip: Bool = data(11)
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
  val mcause = Valid(UInt(64.W))
  val mie = Valid(UInt(64.W))
  val mip = Valid(UInt(64.W))
  val mtvec = Valid(UInt(64.W))
  val mepc = Valid(UInt(64.W))
  val mtval = Valid(UInt(64.W))

  def clear(): Unit = {
    mstatus.valid := false.B
    mcause.valid := false.B
    mie.valid := false.B
    mip.valid := false.B
    mtvec.valid := false.B
    mepc.valid := false.B
    mtval.valid := false.B
    mstatus.bits := 0.U
    mcause.bits := 0.U
    mie.bits := 0.U
    mip.bits := 0.U
    mtvec.bits := 0.U
    mepc.bits := 0.U
    mtval.bits := 0.U
  }
}

class CSRRegs extends Module {
  val io = IO(new Bundle {
    val read_port = Flipped(new CSRReadPort)
    val write_port = Flipped(new CSRWritePort)
    val direct_read_ports = Output(new CSRDirectReadPorts)
    val direct_write_ports = Input(new CSRDirectWritePorts)
    // privilege mode
    val cur_privilege_mode = Input(UInt(2.W))
  })

  // TODO: use config replace hard code
  val smode_enable = false
  val umode_enable = true
  val float_enable = false

  val csr_map = new CSRMap()

  // -----------------------
  // mstatus init value
  // -----------------------
  val mstatus_val = new CSRBitField(0)

  mstatus_val.setField(MStatusMask.MPP, 3) // machine mode
  mstatus_val.setField(MStatusMask.MBE, 0)
  mstatus_val.setField(MStatusMask.SBE, 0)
  mstatus_val.setField(MStatusMask.UBE, 0)

  if (smode_enable) {
    mstatus_val.setField(MStatusMask.SXL, 2) // xlen = 64
  }
  if (umode_enable) {
    mstatus_val.setField(MStatusMask.UXL, 2) // xlen = 64
    mstatus_val.setField(MStatusMask.MPRV, 0)
  }

  println("mstatus_val: " + mstatus_val.getRawValue.toHexString)

  // -----------------
  // mstatus rmask
  // -----------------
  val mstatus_rmask = new CSRBitField(-1L)
  // only support little endian
  mstatus_rmask.setField(MStatusMask.SBE, 0)
  mstatus_rmask.setField(MStatusMask.MBE, 0)
  mstatus_rmask.setField(MStatusMask.UBE, 0)

  if (!smode_enable) {
    mstatus_rmask.setField(MStatusMask.MXR, 0)
    mstatus_rmask.setField(MStatusMask.SUM, 0)
    mstatus_rmask.setField(MStatusMask.TVM, 0)
    mstatus_rmask.setField(MStatusMask.TSR, 0)
  }

  if (!umode_enable) {
    mstatus_rmask.setField(MStatusMask.MPRV, 0)
  }

  if (!umode_enable && !smode_enable) {
    mstatus_rmask.setField(MStatusMask.TW, 0)
  }

  if (!float_enable) {
    mstatus_rmask.setField(MStatusMask.FS, 0)
    mstatus_rmask.setField(MStatusMask.VS, 0)
    mstatus_rmask.setField(MStatusMask.XS, 0)
    mstatus_rmask.setField(MStatusMask.MSTATUS64_SD, 0)
    println("float_enable: " + float_enable)
  }

  println("mstatus_rmask: " + mstatus_rmask.getRawValue.toHexString)

  val mstatus_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(64.W)))
    read_result.valid := true.B
    read_result.bits := reg & Long2UInt64(mstatus_rmask.getRawValue)
    read_result
  }

  // -----------------
  // mstatus wmask
  // -----------------
  val mstatus_wmask = new CSRBitField(mstatus_rmask.getRawValue)
  mstatus_wmask.setField(MStatusMask.UXL, 0)
  mstatus_wmask.setField(MStatusMask.SXL, 0)

  val mstatus_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(64.W)))
    write_result.valid := true.B
    write_result.bits := wdata & Long2UInt64(
      mstatus_wmask.getRawValue
    ) | reg & Long2UInt64(~mstatus_wmask.getRawValue)
    reg := write_result.bits
    write_result
  }

  println("mstatus_wmask: " + mstatus_wmask.getRawValue.toHexString)

  // -----------------
  // misa register
  // -----------------
  val misa_val = new CSRBitField(0)
  misa_val.setField(MIsaMask.I, 1)
  misa_val.setField(MIsaMask.M, 1)
  misa_val.setField(MIsaMask.A, 1)
  misa_val.setField(MIsaMask.C, 1)
  misa_val.setField(MIsaMask.M, 1)
  misa_val.setField(MIsaMask.U, 1)
  misa_val.setField(MIsaMask.MXL, 2) // xlen = 64

  // -----------------
  // csr register
  // -----------------

  println("mstatus_val: " + mstatus_val.getRawValue.toHexString)
  val mstatus = RegInit(Long2UInt64(mstatus_val.getRawValue))
  val mcause = RegInit(0.U(64.W))
  val mie = RegInit(0.U(64.W))
  val mip = RegInit(0.U(64.W))
  val mtvec = RegInit(0.U(64.W))
  val mepc = RegInit(0.U(64.W))
  val mtval = RegInit(0.U(64.W))
  val mscratch = RegInit(0.U(64.W))
  // TODO: make misa configurable
  val misa = RegInit(Long2UInt64(misa_val.getRawValue))
  val mimpid = RegInit(0.U(64.W))
  val mhartid = RegInit(0.U(64.W))
  val marchid = RegInit(0.U(64.W))
  val mvendorid = RegInit(0.U(64.W))

  // counters
  val mcounteren = RegInit(0.U(64.W))

  val cycle = RegInit(0.U(64.W))
  cycle := cycle + 1.U

  // TODO: added but not used now
  val satp = RegInit(0.U(64.W))
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
      (CSRs.satp, satp, normal_read, normal_write),
      (CSRs.mideleg, mideleg, normal_read, normal_write),
      (CSRs.medeleg, medeleg, normal_read, normal_write),
      // read only
      (CSRs.misa, misa, normal_read, empty_write),
      (CSRs.mhartid, mhartid, normal_read, empty_write),
      (CSRs.mimpid, mimpid, normal_read, empty_write),
      (CSRs.marchid, marchid, normal_read, empty_write),
      (CSRs.mvendorid, mvendorid, normal_read, empty_write),
      // counters TODO: not support now
      (CSRs.mcounteren, mcounteren, normal_read, empty_write),
      (CSRs.cycle, cycle, normal_read, empty_write),
      (CSRs.mcycle, cycle, normal_read, empty_write),
      (CSRs.tselect, tselect, normal_read, empty_write)
    )
  m_map.foreach({ case (addr, reg, read_func, write_func) =>
    csr_map.add_csr(addr, reg, read_func, write_func)
  })

  // -----------------------
  // read write logic
  // -----------------------
  io.write_port.write_ex_resp := false.B
  io.read_port.read_ex_resp := false.B
  io.read_port.read_data := 0.U

  def check_csr_permission(addr: UInt, privilege_mode: UInt, is_write: Bool) = {

    assert(addr < 4096.U, "csr addr should be in [0, 4096)")

    val csr_privilege_mode = Wire(UInt(2.W))
    csr_privilege_mode := addr(9, 8)

    val csr_privilege_mode_ok = privilege_mode >= csr_privilege_mode

    val csr_read_only = addr(11, 10) === 3.U

    val csr_ok = csr_privilege_mode_ok && Mux(is_write, !csr_read_only, true.B)
    csr_ok
  }

  when(io.write_port.write_en) {
    when(
      check_csr_permission(io.write_port.addr, io.cur_privilege_mode, true.B)
    ) {
      val w_ret = csr_map.write(io.write_port.addr, io.write_port.write_data)
      io.write_port.write_ex_resp := !w_ret.valid
    }.otherwise {
      io.write_port.write_ex_resp := true.B
    }
  }
  when(io.read_port.read_en) {
    when(
      check_csr_permission(io.read_port.addr, io.cur_privilege_mode, false.B)
    ) {
      val r_ret = csr_map.read(io.read_port.addr)
      io.read_port.read_data := r_ret.bits
      io.read_port.read_ex_resp := !r_ret.valid
    }.otherwise {
      io.read_port.read_ex_resp := true.B
    }
  }

  // -----------------------
  // direct read write logic
  // -----------------------
  io.direct_read_ports.mstatus := mstatus
  io.direct_read_ports.mcause := mcause
  io.direct_read_ports.mie := mie
  io.direct_read_ports.mip := mip
  io.direct_read_ports.mtvec := mtvec
  io.direct_read_ports.mepc := mepc
  io.direct_read_ports.mtval := mtval

  when(io.direct_write_ports.mepc.valid) {
    mepc := io.direct_write_ports.mepc.bits
  }
  when(io.direct_write_ports.mtval.valid) {
    mtval := io.direct_write_ports.mtval.bits
  }
  when(io.direct_write_ports.mtvec.valid) {
    mtvec := io.direct_write_ports.mtvec.bits
  }
  when(io.direct_write_ports.mip.valid) {
    mip := io.direct_write_ports.mip.bits
  }
  when(io.direct_write_ports.mie.valid) {
    mie := io.direct_write_ports.mie.bits
  }
  when(io.direct_write_ports.mcause.valid) {
    mcause := io.direct_write_ports.mcause.bits
  }
  when(io.direct_write_ports.mstatus.valid) {
    mstatus := io.direct_write_ports.mstatus.bits
  }
}

object gen_csr_regs_verilog extends App {
  GenVerilogHelper(new CSRRegs)
}
