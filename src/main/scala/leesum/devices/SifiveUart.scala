package leesum.devices

import chisel3._
import chisel3.util.{Cat, DecoupledIO, Queue, Valid}
import leesum.Utils.{RegManager, SimLog}
import leesum.axi4.BasicMemoryIO
import leesum.{BitMaskHelper, CatReverse, GenVerilogHelper}

object SifiveUartConst {
  val txdata = 0x0
  val rxdata = 0x4
  val txctrl = 0x8
  val rxctrl = 0xc
  val ie = 0x10
  val ip = 0x14
  val div = 0x18
}

class SifiveTXDataField(data: UInt) {
  require(data.getWidth == 32)
  val full = data(31)
  val data_bits = data(7, 0)
}

class SifiveRXDataField(data: UInt) {
  require(data.getWidth == 32)
  val empty = data(31)
  val data_bits = data(7, 0)
}

class SifiveRXCtrlField(data: UInt) {
  require(data.getWidth == 32)
  val rxen = data(0)
  val pad0 = data(15, 1)
  val rxcnt = data(18, 16)
  val pad1 = data(31, 19)
}

class SifiveTXCtrlField(data: UInt) {
  require(data.getWidth == 32)
  val txen = data(0)
  val nstop = data(1)
  val pad0 = data(15, 2)
  val txcnt = data(18, 16)
  val pad1 = data(31, 19)
}

class SifiveIEIPField(data: UInt) {
  require(data.getWidth == 32)
  val txwm = data(0)
  val rxwm = data(1)
  val pad0 = data(31, 2)

  def new_ip(new_txwm: Bool, new_rxwm: Bool) = {
    CatReverse(new_txwm, new_rxwm, 0.U(30.W))
  }
}

class SifiveUart(base_addr: Long) extends Module {

  val io = IO(new Bundle {
    val mem = new BasicMemoryIO(32, 32)

    val irq_out = Output(Bool())
    val tx_deq = DecoupledIO(UInt(8.W))
    val rx_enq = Flipped(DecoupledIO(UInt(8.W)))
  })

  val FIFO_SIZE = 16
  val tx_fifo = Module(new Queue(UInt(8.W), FIFO_SIZE));
  val rx_fifo = Module(new Queue(UInt(8.W), FIFO_SIZE));
  val tx_fifo_full = tx_fifo.io.count === FIFO_SIZE.U
  val rx_fifo_empty = rx_fifo.io.count === 0.U

  tx_fifo.io.enq.noenq()
  rx_fifo.io.deq.nodeq()
  io.tx_deq <> tx_fifo.io.deq
  io.rx_enq <> rx_fifo.io.enq

  val txdata = RegInit(0.U(32.W))
  val rxdata = RegInit(0.U(32.W))
  val txctrl = RegInit(0.U(32.W))
  val rxctrl = RegInit(0.U(32.W))
  val ie = RegInit(0.U(32.W))
  val ip = RegInit(0.U(32.W))
  val div = RegInit(0.U(32.W))
  val irq_out = RegInit(false.B)

  // --------------------------
  // txdata read function
  // --------------------------
  val txdata_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(reg.getWidth.W)))

    when(tx_fifo_full) {
      SimLog(
        desiredName,
        "tx_fifo_full[%d] tx_fifo_len[%d]",
        tx_fifo_full,
        tx_fifo.io.count
      )
    }

    read_result.valid := true.B
    read_result.bits := Cat(tx_fifo_full, 0.U(31.W))
    read_result
  }
  // --------------------------
  // txdata write function
  // --------------------------

  val txdata_write = (addr: UInt, reg: UInt, wdata: UInt) => {
//    Writing to the txdata register enqueues the character contained in the data field to the transmit
//      FIFO if the FIFO is able to accept new entries. Reading from txdata returns the current value of
//      the full flag and zero in the data field. The full flag indicates whether the transmit FIFO is able
//      to accept new entries; when set, writes to data are ignored. A RISC-V amoswap instruction can
//      be used to both read the full status and attempt to enqueue data, with a non-zero return value
//    indicating the character was not accepted
    val write_result = Wire(Valid(UInt(reg.getWidth.W)))
    tx_fifo.io.enq.valid := true.B
    tx_fifo.io.enq.bits := wdata(7, 0)
    write_result.valid := true.B
    write_result.bits := wdata
    write_result
  }
  // --------------------------
  // rxdata read function
  // --------------------------

  val rxdata_read = (addr: UInt, reg: UInt) => {
    //    Reading the rxdata register dequeues a character from the receive FIFO, and returns the value
    //      in the data field. The empty flag indicates if the receive FIFO was empty; when set, the data field
    //    does not contain a valid character. Writes to rxdata are ignored.
    val read_result = Wire(Valid(UInt(reg.getWidth.W)))
    read_result.valid := true.B

    rx_fifo.io.deq.ready := true.B
    read_result.bits := Cat(rx_fifo_empty, 0.U(23.W), rx_fifo.io.deq.bits)
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

  // --------------------------
  // Interrupt
  // --------------------------
  val rxctrl_field = new SifiveRXCtrlField(rxctrl)
  val txctrl_field = new SifiveTXCtrlField(txctrl)
  val ip_field = new SifiveIEIPField(ip)
  val rxwm_pending = rx_fifo.io.count > rxctrl_field.rxcnt;
  val txwm_pending = tx_fifo.io.count < txctrl_field.txcnt;
  ip := ip_field.new_ip(txwm_pending, rxwm_pending)
  irq_out := (ie & ip) =/= 0.U
  io.irq_out := irq_out;
}

object gen_sifive_uart_verilog extends App {
  GenVerilogHelper(new SifiveUart(0x10001000))
}
