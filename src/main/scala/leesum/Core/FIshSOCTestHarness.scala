package leesum.Core

import chisel3._
import chisel3.util.{DecoupledIO, Valid, ValidIO, log2Ceil}
import leesum.axi4._
import leesum.dbg.{DebugModuleConfig, DebugTop, JtagIO}
import leesum.devices.{SifiveUart, clint, plic}
import leesum.moniter.{DifftestPort, PerfPort}
import leesum.{GenVerilogHelper, MSBDivFreq}

class FishSoc(
    // TODO: add config struct
    muldiv_en: Boolean = true,
    rvc_en: Boolean = false
) extends Module {

  val io = IO(new Bundle {
    val difftest = Output(Valid(new DifftestPort(2)))
    val perf_monitor = Output(new PerfPort)
    val mem_port = Flipped(new BasicMemoryIO(32, 64))

    // uart port
    val uart_tx_deq = DecoupledIO(UInt(8.W))
    val uart_rx_enq = Flipped(DecoupledIO(UInt(8.W)))

    // debug
    val jtag_io = new JtagIO(as_master = false)
    val is_halted = Output(Bool())
    val tohost_addr = Input(ValidIO(UInt(64.W)))
  })

  val boot_pc = 0x80000000L
  val mem_addr = 0x80000000L
  val mem_size = 0x8000000L
  val monitor_en = true
  val addr_width = log2Ceil(mem_addr + mem_size)

  val DEVICE_BASE = 0xa0000000L
  val SERIAL_PORT = DEVICE_BASE + 0x00003f8L
  val RTC_ADDR = DEVICE_BASE + 0x0000048L
  val KBD_ADDR = DEVICE_BASE + 0x0000060L
  val VGACTRL_ADDR = DEVICE_BASE + 0x0000100L
  val FB_ADDR = DEVICE_BASE + 0x0100_0000L

  val CLINT_BASE = 0x0200_0000L
  val PLIC_BASE = 0x0c00_0000L
  val SIFIVE_UART_BASE = 0xc000_0000L

  val addr_map = Seq(
    (mem_addr, mem_addr + mem_size, false),
    (SERIAL_PORT, SERIAL_PORT + 0x8, true),
    (RTC_ADDR, RTC_ADDR + 0x8, true),
    (KBD_ADDR, KBD_ADDR + 0x8, true),
    (VGACTRL_ADDR, VGACTRL_ADDR + 0x8, true),
    (FB_ADDR, FB_ADDR + 300 * 400 * 4, true),
    (CLINT_BASE, CLINT_BASE + 0x1_0000, true),
    (PLIC_BASE, PLIC_BASE + 0x0400_0000, true),
    (SIFIVE_UART_BASE, SIFIVE_UART_BASE + 0x1000, true)
  )

  def demux_sel_idx(addr: UInt, en: Bool): UInt = {
    // 0 -> simulated device
    // 1 -> clint
    // 2 -> plic
    // 3 -> sifive_uart
    val addr_decode_map = Seq(
      // simulated device id set to 0
      (mem_addr, mem_addr + mem_size, 0),
      (SERIAL_PORT, SERIAL_PORT + 0x8, 0),
      (RTC_ADDR, RTC_ADDR + 0x8, 0),
      (KBD_ADDR, KBD_ADDR + 0x8, 0),
      (VGACTRL_ADDR, VGACTRL_ADDR + 0x8, 0),
      (FB_ADDR, FB_ADDR + 300 * 400 * 4, 0),
      // real device
      (CLINT_BASE, CLINT_BASE + 0x1_0000, 1),
      (PLIC_BASE, PLIC_BASE + 0x0400_0000, 2),
      (SIFIVE_UART_BASE, SIFIVE_UART_BASE + 0x1000, 3)
    )
    val addr_decoder = Module(
      new AddrDecoder(addr_decode_map)
    )
    addr_decoder.io.addr.valid := en
    addr_decoder.io.addr.bits := addr
    assert(
      addr_decoder.io.sel_error === false.B,
      "addr_decoder sel error: %x\n",
      addr
    )
    addr_decoder.io.sel_idx
  }

  val core = Module(
    new FishCore(muldiv_en, rvc_en, monitor_en, boot_pc, addr_map)
  )

  core.io.tohost_addr := io.tohost_addr

  val dm_config = new DebugModuleConfig
  val debug_top = Module(
    new DebugTop(dm_config)
  )

  core.io.difftest <> io.difftest
  core.io.perf_monitor <> io.perf_monitor

  debug_top.io.jtag_io <> io.jtag_io
  debug_top.io.debug_core_interface <> core.io.debug_core_interface

  io.is_halted := core.io.debug_core_interface.state_regs.is_halted

  val axi_demux = Module(
    new AXIDeMux(4, 32, 64)
  )

  // core <> axi_demux
  core.io.axi_master <> axi_demux.io.in

  axi_demux.io.r_sel := demux_sel_idx(
    core.io.axi_master.ar.bits.addr,
    core.io.axi_master.ar.valid
  )
  axi_demux.io.w_sel := demux_sel_idx(
    core.io.axi_master.aw.bits.addr,
    core.io.axi_master.aw.valid
  )

  // -------------------
  // rtc clk
  // -------------------
  val rtc_clk = Module(new MSBDivFreq(64))

  // -------------------
  // devices
  // -------------------

  // mem_port <>  mem_axi_bridge <> axi_demux(0)
  val mem_axi_bridge = Module(
    new AXI4SlaveBridge(
      32,
      64
    )
  )
  io.mem_port <> mem_axi_bridge.io.mem_port
  mem_axi_bridge.io.axi_slave <> axi_demux.io.out(0)

  // clint <> clint_axi_bridge <> axi_demux(1)
  val clint = Module(new clint(1, 0x2000000))
  val clint_axi_bridge = Module(
    new AXI4SlaveBridge(
      32,
      64
    )
  )
  clint.io.rtc_clk := rtc_clk.io.clk_div
  clint.io.mem <> clint_axi_bridge.io.mem_port
  clint_axi_bridge.io.axi_slave <> axi_demux.io.out(1)

  // clint <> core
  core.io.mtime := clint.io.mtime
  core.io.time_int := clint.io.time_int.head
  core.io.soft_int := clint.io.soft_int.head

  // plic32 <> 32to64 <> plic_axi_bridge <> axi_demux(2)
  val harts_map = Seq(true) // true: smode enable,
  val plic32 = Module(
    new plic(harts_map, PLIC_BASE.toInt, 15)
  ) // sifive uart irq: 10
  plic32.io.irq_pendings.foreach(_ := false.B)

  core.io.mext_int := plic32.io.harts_ext_irq(0)(0)
  core.io.sext_int := plic32.io.harts_ext_irq(0)(1)

  val plic32to64 = Module(
    new MemoryIO64to32(
      32
    )
  )
  plic32to64.io.after <> plic32.io.mem
  val plic_axi_bridge = Module(
    new AXI4SlaveBridge(
      32,
      64
    )
  )
  plic_axi_bridge.io.mem_port <> plic32to64.io.before
  plic_axi_bridge.io.axi_slave <> axi_demux.io.out(2)

  // ---------------------------------------------------------------------
  // SifiveUart module
  // sifive_uart32 <> 32to64 <> sifive_uart_axi_bridge <> axi_demux(3)
  // ---------------------------------------------------------------------
  val sifive_uart32 = Module(new SifiveUart(SIFIVE_UART_BASE))
  sifive_uart32.io.tx_deq <> io.uart_tx_deq
  io.uart_rx_enq <> sifive_uart32.io.rx_enq

  plic32.io.irq_pendings(10) := sifive_uart32.io.irq_out

  val sifive_uart32to64 = Module(
    new MemoryIO64to32(
      32
    )
  )
  sifive_uart32to64.io.after <> sifive_uart32.io.mem
  val sifive_uart_axi_bridge = Module(
    new AXI4SlaveBridge(
      32,
      64
    )
  )
  sifive_uart_axi_bridge.io.mem_port <> sifive_uart32to64.io.before
  sifive_uart_axi_bridge.io.axi_slave <> axi_demux.io.out(3)

}

object gen_FishSoc extends App {
  val projectDir = System.getProperty("user.dir")
  GenVerilogHelper(
    new FishSoc(
      muldiv_en = true,
      rvc_en = true
    ),
    s"$projectDir/sim/vsrc/ysyx_v2.sv"
  )
}

/** This is a simplified version of CoreDutFull, which is used for VIVADO STA.
  */
object gen_CoreTestSTA extends App {
  val projectDir = System.getProperty("user.dir")
  GenVerilogHelper(
    new FishSoc(
      muldiv_en = false,
      rvc_en = true
    ),
    "/home/leesum/vivado_project/ysyx_v2/ysyx_v2.srcs/sources_1/imports/ysyx_v2/ysyx_v2.sv"
  )
}
