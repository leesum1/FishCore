package leesum
import chisel3.util.{Cat, MuxLookup, Valid}
import chisel3.{Reg, _}

class CSRMap {

  val csr_map = collection.mutable
    .Map[
      Int,
      (UInt, (UInt, UInt) => Valid[UInt], (UInt, UInt) => Valid[UInt])
    ]()

  val normal_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(64.W)))
    read_result.valid := true.B
    read_result.bits := reg
    read_result
  }
  val normal_write = (addr: UInt, reg: UInt) => {
    val write_result = Wire(Valid(UInt(64.W)))
    write_result.valid := true.B
    write_result.bits := reg
    write_result
  }

  val deny_write = (addr: UInt, reg: UInt) => {
    val write_result = Wire(Valid(UInt(64.W)))
    write_result.valid := false.B
    write_result.bits := reg
    write_result
  }

  def add_csr(
      addr: Int,
      reg: UInt,
      read_func: (UInt, UInt) => Valid[UInt] = normal_read,
      write_func: (UInt, UInt) => Valid[UInt] = normal_write
  ) = {

    require(addr >= 0 && addr < 4096, s"csr addr $addr should be in [0, 4096)")
    require(!csr_map.contains(addr), s"csr addr $addr already exists")

    csr_map.addOne(addr, (reg, read_func, write_func))
  }
  def read(raddr: UInt): Valid[UInt] = {
    val raddr_map = csr_map.map({ case (addr, (reg, read_func, _)) =>
      val read_result = read_func(addr.U, reg)
      (addr.U, read_result)
    })

    val default_read_result = Wire(Valid(UInt(64.W)))
    default_read_result.valid := false.B
    default_read_result.bits := 0.U

    val rdata = MuxLookup(raddr, default_read_result)(
      raddr_map.toSeq
    )
    rdata
  }

  def write(waddr: UInt, wdata: UInt): Valid[UInt] = {

    val result = Wire(Valid(UInt(64.W)))
    result.valid := false.B
    result.bits := 0.U
    csr_map.foreach({ case (addr, (reg, _, write_func)) =>
      when(waddr === addr.U) {
        val write_result = write_func(addr.U, wdata)
        reg := write_result.bits
        result := write_result
      }
    })
    result
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

  def sie: Bool = data(1)
  def mie: Bool = data(3)

  def spie: Bool = data(5)
  def mpie: Bool = data(7)
  def spp: Bool = data(8)
  def vs: UInt = data(10, 9)
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

  def get_exception_mstatus(cur_privilege: UInt): UInt = {
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

  // TODO: need implement
  def get_mret_mstatus(clear_mprv: Bool): UInt = {
    val new_mstatus = Cat(
      data(63, 18),
      Mux(clear_mprv, false.B, this.mprv), // mprv
      data(16, 13),
      // mpp , xPP is set to the least-privileged supported mode (U if U-mode is implemented, else M).
      Privilegelevel.M.U(2.W),
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
  })

  val csr_map = new CSRMap()

  val mstatus = RegInit(Long2UInt64(0x0000000000001800L))
  val mcause = RegInit(0.U(64.W))
  val mie = RegInit(0.U(64.W))
  val mip = RegInit(0.U(64.W))
  val mtvec = RegInit(0.U(64.W))
  val mepc = RegInit(0.U(64.W))
  val mtval = RegInit(0.U(64.W))
  val mscratch = RegInit(0.U(64.W))
  val misa = RegInit(Long2UInt64(0x8000000000001100L))

  val m_map =
    Seq(
      (CSRs.mstatus, mstatus),
      (CSRs.mcause, mcause),
      (CSRs.mie, mie),
      (CSRs.mtvec, mtvec),
      (CSRs.mtval, mtval),
      (CSRs.mepc, mepc),
      (CSRs.mip, mip),
      (CSRs.mscratch, mscratch),
      (CSRs.misa, misa)
    )
  m_map.foreach({ case (addr, reg) =>
    csr_map.add_csr(addr, reg)
  })

  // -----------------------
  // read write logic
  // -----------------------
  io.write_port.write_ex_resp := false.B
  io.read_port.read_ex_resp := false.B
  io.read_port.read_data := 0.U

  when(io.write_port.write_en) {
    val w_ret = csr_map.write(io.write_port.addr, io.write_port.write_data)
    io.write_port.write_ex_resp := !w_ret.valid
  }
  when(io.read_port.read_en) {
    val r_ret = csr_map.read(io.read_port.addr)
    io.read_port.read_data := r_ret.bits
    io.read_port.read_ex_resp := !r_ret.valid
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
