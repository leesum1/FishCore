package leesum.moniter
import chisel3._
import chisel3.util.{Decoupled, Enum, PopCount, PriorityMux, Valid, is, switch}
import leesum.{CSRDirectReadPorts, ExceptionEntry, FuOP, FuType}

class GERMonitorPort extends Bundle {
  val gpr = Vec(32, UInt(64.W))
}
class CommitMonitorPort extends Bundle {
  val pc = UInt(64.W)
  val inst = UInt(32.W)
  val fu_type = FuType()
  val fu_op = FuOP()
  val exception = new ExceptionEntry()
  val is_mmio = Bool()
}

class DifftestCsr extends Bundle {}

class DifftestPort extends Bundle {
  val pc = UInt(64.W)
  val inst = UInt(32.W)
  val exception = new ExceptionEntry()
  val commited_num = UInt(8.W)
  val contain_mmio = Bool()
  // csr port
  val csr = new CSRDirectReadPorts
  val gpr = Vec(32, UInt(64.W))
}
class MonitorTop(commit_port_num: Int) extends Module {
  val io = IO(new Bundle {
    val commit_monitor =
      Vec(commit_port_num, Flipped(Valid(new CommitMonitorPort)))
    val gpr_monitor = Input(new GERMonitorPort)
    val csr_monitor = Input(new CSRDirectReadPorts)
    val difftest = Output(Valid(new DifftestPort))
  })
  // delay 1 cycle to wait for gpr write back
  val commit_monitor_next = RegNext(io.commit_monitor)
  val commit_monitor_count = PopCount(commit_monitor_next.map(_.valid))

  val last_commit_inst = PriorityMux(
    commit_monitor_next.map(_.valid).reverse,
    commit_monitor_next.map(_.bits).reverse
  )
  val last_exception = PriorityMux(
    commit_monitor_next.map(_.valid).reverse,
    commit_monitor_next.map(_.bits.exception).reverse
  )

  val contain_mmio =
    commit_monitor_next.map(x => x.valid && x.bits.is_mmio).reduce(_ || _)

  io.difftest.valid := commit_monitor_count > 0.U
  io.difftest.bits.gpr := io.gpr_monitor.gpr
  io.difftest.bits.csr := io.csr_monitor
  io.difftest.bits.commited_num := commit_monitor_count
  io.difftest.bits.pc := last_commit_inst.pc
  io.difftest.bits.inst := last_commit_inst.inst
  io.difftest.bits.exception := last_exception
  io.difftest.bits.contain_mmio := contain_mmio & !last_exception.valid
}
