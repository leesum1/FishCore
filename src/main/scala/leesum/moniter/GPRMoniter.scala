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
  val is_rvc = Bool()
  val fu_type = UInt(FuType.width.W)
  val fu_op = UInt(FuOP.width.W)
  val exception = new ExceptionEntry()
  val is_mmio = Bool()
  val csr_skip = Bool()
  val has_interrupt = Bool()
}

class DifftestCsr extends Bundle {}

class DifftestPort extends Bundle {
  val pc = UInt(64.W)
  val inst = UInt(32.W)
  val is_rvc = Bool()
  val exception = new ExceptionEntry()
  val commited_num = UInt(8.W)
  val contain_mmio = Bool()
  val has_interrupt = Bool()
  val csr_skip = Bool()
  // csr port
  val csr = new CSRDirectReadPorts
  val gpr = Vec(32, UInt(64.W))
}

class PerfPort extends Bundle {
  val bp = new PerfMonitorCounter
  val commit = new PerfMonitorCounter
  val icache = new PerfMonitorCounter
  val dcache = new PerfMonitorCounter
  val itlb = new PerfMonitorCounter
  val dtlb = new PerfMonitorCounter
}

class MonitorTop(commit_port_num: Int) extends Module {
  val io = IO(new Bundle {
    val commit_monitor =
      Vec(commit_port_num, Flipped(Valid(new CommitMonitorPort)))
    val gpr_monitor = Input(new GERMonitorPort)
    val csr_monitor = Input(new CSRDirectReadPorts)
    val difftest = Output(Valid(new DifftestPort))
    // perf monitor
    val perf_bp = Input(new PerfMonitorCounter)
    val perf_commit = Input(new PerfMonitorCounter)
    val perf_icache = Input(new PerfMonitorCounter)
    val perf_dcache = Input(new PerfMonitorCounter)
    val perf_itlb = Input(new PerfMonitorCounter)
    val perf_dtlb = Input(new PerfMonitorCounter)

    val perf = Output(new PerfPort)
  })

  // ------------------------
  // perf monitor
  // ------------------------
  io.perf.bp := io.perf_bp
  io.perf.commit := io.perf_commit
  io.perf.icache := io.perf_icache
  io.perf.dcache := io.perf_dcache
  io.perf.itlb := io.perf_itlb
  io.perf.dtlb := io.perf_dtlb

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

  val has_interrupt =
    commit_monitor_next.map(x => x.valid && x.bits.has_interrupt).reduce(_ || _)
  val csr_skip =
    commit_monitor_next.map(x => x.valid && x.bits.csr_skip).reduce(_ || _)

  io.difftest.valid := commit_monitor_count > 0.U
  io.difftest.bits.gpr := io.gpr_monitor.gpr
  io.difftest.bits.csr := io.csr_monitor
  io.difftest.bits.commited_num := commit_monitor_count
  io.difftest.bits.pc := last_commit_inst.pc
  io.difftest.bits.is_rvc := last_commit_inst.is_rvc
  io.difftest.bits.inst := last_commit_inst.inst
  io.difftest.bits.exception := last_exception
  io.difftest.bits.contain_mmio := contain_mmio & !last_exception.valid
  io.difftest.bits.has_interrupt := has_interrupt & !last_exception.valid
  io.difftest.bits.csr_skip := csr_skip & !last_exception.valid
}
