package leesum.devices

import chisel3._
import chisel3.util.{Cat, Valid}
import leesum.Utils.RegManager
import leesum.axi4.BasicMemoryIO
import leesum.GenVerilogHelper

object SifiveUartConst {
  val txdata = 0x0
  val rxdata = 0x4
  val txctrl = 0x8
  val rxctrl = 0xc
  val ie = 0x10
  val ip = 0x14
  val div = 0x18
}

class UartIO extends Bundle {
  val tx_data = Output(Valid(UInt(8.W)))
  val rx_data = Input((Valid(UInt(8.W))))

  def as_master(): UartIO = {
    this
  }

  def as_slave(): UartIO = {
    Flipped(this)
  }

}

class SifiveUart(base_addr: Long) extends Module {

  val io = IO(new Bundle {
    val mem = new BasicMemoryIO(32, 32)
    val irq_out = Output(Bool())
    val uart_io = (new UartIO).as_master()
  })

  io.uart_io.tx_data.valid := false.B
  io.uart_io.tx_data.bits := DontCare

  io.irq_out := false.B

  val txdata = RegInit(0.U(32.W))
  val rxdata = RegInit(0.U(32.W))
  val txctrl = RegInit(0.U(32.W))
  val rxctrl = RegInit(0.U(32.W))
  val ie = RegInit(0.U(32.W))
  val ip = RegInit(0.U(32.W))
  val div = RegInit(0.U(32.W))

  // --------------------------
  // txdata read function
  // --------------------------
  val txdata_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(reg.getWidth.W)))
    read_result.valid := true.B
    // TODO: full bis is always false
    read_result.bits := Cat(false.B, reg(30, 0))
    read_result
  }
  // --------------------------
  // txdata write function
  // --------------------------

  val txdata_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(reg.getWidth.W)))

    io.uart_io.tx_data.valid := true.B
    io.uart_io.tx_data.bits := wdata(7, 0)

    write_result.valid := true.B
    write_result.bits := wdata
    write_result
  }
  // --------------------------
  // rxdata read function
  // --------------------------

  val rxdata_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(reg.getWidth.W)))
    read_result.valid := true.B

    // TODO: empty bis is always true, don't support read now
    read_result.bits := Cat(true.B, reg(30, 0))
    read_result
  }

  val sifive_uart_regs = new RegManager

  val reg_map =
    Seq(
      (
        base_addr + SifiveUartConst.txdata,
        txdata,
        txdata_read,
        txdata_write
      ),
      (
        base_addr + SifiveUartConst.rxdata,
        rxdata,
        rxdata_read,
        sifive_uart_regs.empty_write
      ),
      (
        base_addr + SifiveUartConst.txctrl,
        txctrl,
        sifive_uart_regs.normal_read,
        sifive_uart_regs.normal_write
      ),
      (
        base_addr + SifiveUartConst.rxctrl,
        rxctrl,
        sifive_uart_regs.normal_read,
        sifive_uart_regs.normal_write
      ),
      (
        base_addr + SifiveUartConst.ie,
        ie,
        sifive_uart_regs.normal_read,
        sifive_uart_regs.normal_write
      ),
      (
        base_addr + SifiveUartConst.ip,
        ip,
        sifive_uart_regs.normal_read,
        sifive_uart_regs.empty_write
      ),
      (
        base_addr + SifiveUartConst.div,
        div,
        sifive_uart_regs.normal_read,
        sifive_uart_regs.normal_write
      )
    )

  reg_map.foreach { case (addr, reg, read_func, write_func) =>
    sifive_uart_regs.add_reg(addr, reg, read_func, write_func)
  }

  sifive_uart_regs.print_map()

  // --------------------------
  // read
  // --------------------------
  val last_read = RegInit(0.U(32.W))
  when(io.mem.i_rd) {
    val read_result = Wire(Valid(UInt(32.W)))
    read_result.valid := true.B
    read_result.bits := sifive_uart_regs.read(io.mem.i_raddr).bits
    assert(
      sifive_uart_regs.in_range(io.mem.i_raddr),
      "sifive_uart read out of range"
    )
    last_read := read_result.bits
  }
  io.mem.o_rdata := last_read
  // --------------------------
  // write
  // --------------------------
  when(io.mem.i_we) {
    sifive_uart_regs.write(io.mem.i_waddr, io.mem.i_wdata)
    assert(
      sifive_uart_regs.in_range(io.mem.i_waddr),
      "sifive_uart write out of range"
    )
  }
}

object gen_sifive_uart_verilog extends App {
  GenVerilogHelper(new SifiveUart(0x10001000))
}
