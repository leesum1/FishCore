package leesum
import chisel3._
import leesum.moniter.GERMonitorPort

class RegFile {
//  val rf = Mem(32, UInt(64.W))
  val rf = RegInit(VecInit(Seq.fill(32)(0.U(64.W))))
  def read(addr: UInt): UInt = {
    require(addr.getWidth == 5)
    rf(addr)
  }
  def write(addr: UInt, data: UInt) = {
    require(addr.getWidth == 5)
    when(addr =/= 0.U) {
      rf(addr) := data
    }
  }
}

class GPRsWritePort extends Bundle {
  val addr = Input(UInt(5.W))
  val wdata = Input(UInt(64.W))
  val wen = Input(Bool())

  def write(addr: UInt, data: UInt) = {
    this.addr := addr
    this.wdata := data
    this.wen := true.B
  }
}

class RegFileReadPort extends Bundle {
  val rs1_addr = Output(UInt(5.W))
  val rs2_addr = Output(UInt(5.W))
  val rs1_data = Input(UInt(64.W))
  val rs2_data = Input(UInt(64.W))
}

class GPRs(read_port_num: Int, write_port_num: Int, monitor_en: Boolean = false)
    extends Module {
  val io = IO(new Bundle {
    val read_ports = Vec(read_port_num, Flipped(new RegFileReadPort))
    val write_ports = Vec(write_port_num, new GPRsWritePort)

    val gpr_monitor = if (monitor_en) Some(Output(new GERMonitorPort)) else None
  })

  val rf = new RegFile

  for (i <- 0 until read_port_num) {
    io.read_ports(i).rs1_data := rf.read(io.read_ports(i).rs1_addr)
    io.read_ports(i).rs2_data := rf.read(io.read_ports(i).rs2_addr)
  }

  // if multiple write ports write to the same address, the last one will take effect
  for (i <- 0 until write_port_num) {
    when(io.write_ports(i).wen) {
      rf.write(io.write_ports(i).addr, io.write_ports(i).wdata)
    }
  }
  // -----------------------
  // gpr_monitor
  // -----------------------

  if (monitor_en) {
    io.gpr_monitor.get.gpr := rf.rf
  }

  // -----------------------
  // assert
  // -----------------------

}

object gen_gprs_verilog extends App {
  GenVerilogHelper(
    new GPRs(read_port_num = 2, write_port_num = 2, monitor_en = true)
  )
}
