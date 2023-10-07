package leesum
import chisel3._
import chisel3.util.{Decoupled, Valid, isPow2, log2Ceil}

class RegFile {
  val rf = Mem(32, UInt(64.W))
//  val rf = RegInit(VecInit(Seq.fill(32)(0.U(64.W))))
  def read(addr: UInt): UInt = {
    require(addr.getWidth == 5)
    Mux(addr === 0.U, 0.U, rf(addr))
  }
  def write(addr: UInt, data: UInt) = {
    require(addr.getWidth == 5)
    rf(addr) := data
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

class GPRs(read_port_num: Int, write_port_num: Int) extends Module {
  val io = IO(new Bundle {
    val read_ports = Vec(read_port_num, Flipped(new RegFileReadPort))
    val write_ports = Vec(write_port_num, new GPRsWritePort)
  })

  val rf = new RegFile

  for (i <- 0 until read_port_num) {
    io.read_ports(i).rs1_data := rf.read(io.read_ports(i).rs1_addr)
    io.read_ports(i).rs2_data := rf.read(io.read_ports(i).rs2_addr)
  }
  for (i <- 0 until write_port_num) {
    when(io.write_ports(i).wen) {
      rf.write(io.write_ports(i).addr, io.write_ports(i).wdata)
    }
  }
  // -----------------------
  // assert
  // -----------------------

  assert(
    CheckUnique(
      VecInit(io.write_ports.map(wp => Mux(wp.wen, wp.addr, 0.U))),
      0.U
    ),
    "write address conflict"
  )
}

object gen_gprs_verilog extends App {
  GenVerilogHelper(
    new GPRs(read_port_num = 2, write_port_num = 4)
  )
}
