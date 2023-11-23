package leesum.devices

import chisel3._
import chisel3.util.{Cat, Valid}
import leesum.axi4.BasicMemoryIO
import leesum.{GenVerilogHelper, RegMap}

//const MSIP_BASE: u64 = 0x0;
//const MSIP_PER_HART: u64 = 0x4;
//const MSIP_END: u64 = MTIMECMP_BASE - 1;
//
//const MTIMECMP_BASE: u64 = 0x4000;
//const MTIMECMP_PER_HART: u64 = 0x8;
//const MTIMECMP_END: u64 = MTIME_BASE - 1;
//const MTIME_BASE: u64 = 0xBFF8;
//const MTIME_BASE_END: u64 = 0xBFF8 + 7;
object ClintConst {
  val msip = 0x0
  val msip_per_hart = 0x4

  val mtimecmp = 0x4000
  val mtimecmp_per_hart = 0x8
  val mtime = 0xbff8

}

class clint(harts_num: Int = 1, device_base: Int) extends Module {

  val io = IO(new Bundle {
    val mem = new BasicMemoryIO(32, 64)
    val mtime = Output(UInt(64.W))
    val mtimecmp = Output(Vec(harts_num, UInt(64.W)))
    val time_int = Output(Vec(harts_num, Bool()))
    val soft_int = Output(Vec(harts_num, Bool()))
  })

  val clint_regs = new RegMap

  // TODO: not correct now !!!!!!!!1
  val msip = RegInit(VecInit(Seq.fill(harts_num)(0.U(64.W))))
  val mtimecmp = RegInit(VecInit(Seq.fill(harts_num)(0.U(64.W))))

  val mtime = RegInit(0.U(64.W))

  val has_overflow = VecInit(mtimecmp.map(mtime >= _))
  val has_soft_int = VecInit(msip.map(_ =/= 0.U))

  io.soft_int := has_soft_int
  io.time_int := has_overflow

  mtime := mtime + 1.U

  io.mtime := mtime
  io.mtimecmp := mtimecmp

  clint_regs.add_reg(
    device_base + ClintConst.mtime,
    mtime,
    clint_regs.normal_read,
    clint_regs.empty_write
  )

  for (i <- 0 until harts_num) {
    clint_regs.add_reg(
      device_base + ClintConst.msip + i * ClintConst.msip_per_hart,
      msip(i),
      clint_regs.normal_read,
      clint_regs.normal_write
    )
    clint_regs.add_reg(
      device_base + ClintConst.mtimecmp + i * ClintConst.mtimecmp_per_hart,
      mtimecmp(i),
      clint_regs.normal_read,
      clint_regs.normal_write
    )
  }

  clint_regs.print_map()

  val last_read = RegInit(0.U(64.W))

  when(io.mem.i_rd) {
    last_read := clint_regs.read(io.mem.i_raddr).bits
    assume(clint_regs.in_range(io.mem.i_raddr))
  }

  when(io.mem.i_we) {
    clint_regs.write(io.mem.i_waddr, io.mem.i_wdata)
    assume(clint_regs.in_range(io.mem.i_waddr))
  }

  io.mem.o_rdata := last_read
}

object gen_clint_verilog extends App {
  GenVerilogHelper(new clint(1, 0x2000000))
}
