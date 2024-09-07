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

  // ---------------------
  // dcsr write func
  // ---------------------

  val dcsr_write_func = (addr: UInt, reg: UInt, wdata: UInt) => {
    val dcsr_field = new DCSRFiled(reg(31, 0))
    val write_result = Wire(Valid(UInt(64.W)))
    val new_dcsr = dcsr_field.get_wirte_dcsr(wdata(31, 0))
    write_result.valid := true.B
    write_result.bits := Cat(0.U(32.W), new_dcsr)
    reg := write_result.bits
    write_result
  }

  // -----------------
  // misa Init value
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

  println("misa_val: " + misa_val.get_raw.toHexString)

  // -----------------
  // dcsr Init value
  // -----------------
  val dcsr_val = new CSRBitField(0)
  dcsr_val.set_field(DCSRMask.debugver, 4) // has debug module
  dcsr_val.set_field(DCSRMask.stopcount, 0) // Increment counters as usual.
  // Interrupts (includingNMI) are disabled during single stepping.
  dcsr_val.set_field(DCSRMask.stepie, 0)
  dcsr_val.set_field(DCSRMask.stoptime, 0) // time continues to reflect mtime.
  dcsr_val.set_field(DCSRMask.prv, 3) // machine mode

  dcsr_val.set_field(DCSRMask.mprven, 1)

  println("dcsr_val: " + dcsr_val.get_raw.toHexString)

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

  val dcsr = RegInit(Long2UInt64(dcsr_val.get_raw))
  val dpc = RegInit(0.U(64.W))
  val dscratch0 = RegInit(0.U(64.W))
  val dscratch1 = RegInit(0.U(64.W))

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
      (CSRs.dcsr, dcsr, normal_read, dcsr_write_func),
      (CSRs.dpc, dpc, normal_read, normal_write),
      (CSRs.dscratch0, dscratch0, normal_read, normal_write),
      (CSRs.dscratch1, dscratch1, normal_read, normal_write),

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
