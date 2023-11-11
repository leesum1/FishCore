package leesum.Core
import chisel3._
import chisel3.util.{Valid, log2Ceil}
import leesum.axi4.{AXI4SlaveBridge, AxiReadArbiter, BasicMemoryIO}
import leesum.moniter.{DifftestPort, MonitorTop}
import leesum._
class CoreDutFull(
    random_latency: Boolean = false,
    muldiv_en: Boolean = true,
    rvc_en: Boolean = false
) extends Module {
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
  val FB_ADDR = DEVICE_BASE + 0x1000000L

  val addr_map = Seq(
    (mem_addr, mem_addr + mem_size, false),
    (SERIAL_PORT, SERIAL_PORT + 0x8, true),
    (RTC_ADDR, RTC_ADDR + 0x8, true),
    (KBD_ADDR, KBD_ADDR + 0x8, true),
    (VGACTRL_ADDR, VGACTRL_ADDR + 0x8, true),
    (FB_ADDR, FB_ADDR + 300 * 400 * 4, true)
  )

  val io = IO(new Bundle {
    val difftest = Output(Valid(new DifftestPort))
    val mem_port = Flipped(new BasicMemoryIO(addr_width, 64))
  })

  // monitor
  val monitor = Module(new MonitorTop(2))
  io.difftest <> monitor.io.difftest

  // pipeline stage
  val pc_gen_stage = Module(new PCGenStage(boot_pc, rvc_en))
  val fetch_stage = Module(new FetchStage)
  val inst_realign = Module(new InstReAlign(rvc_en))
  val inst_fifo = Module(new InstsFIFO)

  val decode_stage = Seq.tabulate(2)(i => Module(new InstDecoder))

  val rob = Module(new ReOrderBuffer(8, 2, 2))
  val issue_stage_rob = Module(new IssueStageNew(2, 2))

  val commit_stage = Module(new CommitStage(2, monitor_en))
  val reg_file = Module(new GPRs(2, 2, monitor_en))
  val csr_regs = Module(new CSRRegs)

  val fetch_tlb = Module(new DummyTLB(random_latency))
  val lsu_tlb = Module(new DummyTLB(random_latency))

  // fu
  val alu_seq = Seq.fill(2)(Module(new FuAlu))
  val lsu = Module(new LSU(addr_map))
  val bru = Module(new FuBranch(rvc_en))
  val mul_div = Module(new FuMulDiv(muldiv_en))
  val csr = Module(new FuCSR)

  val dcache = Module(new DummyDCache)
  val icache = Module(new DummyICache)
  val axi_r_arb = Module(new AxiReadArbiter)
  val axi_mem = Module(
    new AXI4SlaveBridge(
      AXI_AW = 32,
      AXI_DW = 64,
      INTERNAL_MEM_SIZE = mem_size,
      INTERNAL_MEM_DW = 64,
      INTERNAL_MEM_BASE = mem_addr
    )
  )

  axi_mem.io.mem_port <> io.mem_port

  axi_r_arb.io.in.foreach(axi => {
    axi.aw.noenq()
    axi.w.noenq()
    axi.b.nodeq()
  })

  axi_r_arb.io.out.aw.nodeq()
  axi_r_arb.io.out.w.nodeq()
  axi_r_arb.io.out.b.noenq()

  icache.io.axi_mem.ar <> axi_r_arb.io.in(1).ar
  icache.io.axi_mem.r <> axi_r_arb.io.in(1).r
  icache.io.axi_mem.aw.nodeq()
  icache.io.axi_mem.w.nodeq()
  icache.io.axi_mem.b.noenq()

  dcache.io.axi_mem.ar <> axi_r_arb.io.in(0).ar
  dcache.io.axi_mem.r <> axi_r_arb.io.in(0).r

  axi_mem.io.axi_slave.ar <> axi_r_arb.io.out.ar
  axi_mem.io.axi_slave.r <> axi_r_arb.io.out.r
  axi_mem.io.axi_slave.aw <> dcache.io.axi_mem.aw
  axi_mem.io.axi_slave.w <> dcache.io.axi_mem.w
  axi_mem.io.axi_slave.b <> dcache.io.axi_mem.b

  // flush
  axi_r_arb.io.flush := commit_stage.io.flush
  fetch_stage.io.flush := commit_stage.io.flush
  inst_fifo.io.flush := commit_stage.io.flush
  issue_stage_rob.io.flush := commit_stage.io.flush
  rob.io.flush := commit_stage.io.flush
  lsu.io.flush := commit_stage.io.flush
  fetch_tlb.io.flush := commit_stage.io.flush
  lsu_tlb.io.flush := commit_stage.io.flush
  dcache.io.flush := commit_stage.io.flush
  icache.io.flush := commit_stage.io.flush
  inst_realign.io.flush := commit_stage.io.flush
  bru.io.flush := commit_stage.io.flush
  mul_div.io.flush := commit_stage.io.flush
  csr.io.flush := commit_stage.io.flush

  // pc_gen_stage <> fetch_stage
  pc_gen_stage.io.pc <> fetch_stage.io.pc_in

  // fetch_stage <> inst_realign
  inst_realign.unaligned_insts.valid := fetch_stage.io.fetch_resp.valid
  fetch_stage.io.fetch_resp.ready := inst_realign.unaligned_insts.ready
  inst_realign.unaligned_insts.bits.pc := fetch_stage.io.fetch_resp.bits.pc
  inst_realign.unaligned_insts.bits.payload := fetch_stage.io.fetch_resp.bits.data

  // fetch_stage <> tlb_arb
  fetch_tlb.io.tlb_req <> fetch_stage.io.tlb_req
  fetch_tlb.io.tlb_resp <> fetch_stage.io.tlb_resp

  // fetch_stage <> icache
  fetch_stage.io.icache_req <> icache.io.load_req
  fetch_stage.io.icache_resp <> icache.io.load_resp

  // inst_realign <> inst_fifo
  inst_fifo.io.push <> inst_realign.io.output

  // inst_fifo <> decode stage

  for (i <- decode_stage.indices) {
    inst_fifo.io.pop(i).ready := decode_stage(i).io.in.ready
    decode_stage(i).io.in.valid := inst_fifo.io.pop(i).valid
    decode_stage(i).io.in.bits.inst := inst_fifo.io.pop(i).bits.inst
    decode_stage(i).io.in.bits.pc := inst_fifo.io.pop(i).bits.pc
    decode_stage(i).io.in.bits.is_rvc := inst_fifo.io.pop(i).bits.rvc
    decode_stage(i).io.in.bits.is_valid := inst_fifo.io.pop(i).bits.valid
    decode_stage(i).io.in.bits.inst_c := inst_fifo.io.pop(i).bits.inst_c
    decode_stage(i).io.in.bits.exception := DontCare
    decode_stage(i).io.in.bits.bp := DontCare
    decode_stage(i).io.in.bits.exception.valid := false.B
    decode_stage(i).io.in.bits.bp.is_taken := false.B
  }

  // decode stage <> issue stage
  for (i <- decode_stage.indices) {
    decode_stage(i).io.out <> issue_stage_rob.io.push_port(i)
  }

  // issue stage <> scoreboard
  issue_stage_rob.io.pop_port <> rob.io.push_ports
  issue_stage_rob.io.alloc_trans_id <> rob.io.push_trans_id
  issue_stage_rob.io.operand_bypass_resp := rob.io.operand_bypass_resp
  rob.io.operand_bypass_req := issue_stage_rob.io.operand_bypass_req

  // issue stage <> fu
  require(issue_stage_rob.io.fu_port.alu.length == alu_seq.length)
  for (i <- alu_seq.indices) {
    issue_stage_rob.io.fu_port.alu(i) <> alu_seq(i).io.in
  }
  issue_stage_rob.io.fu_port.lsu_0 <> lsu.io.lsu_req
  issue_stage_rob.io.fu_port.branch_0 <> bru.io.in
  issue_stage_rob.io.fu_port.mul_0 <> mul_div.io.mul_req
  issue_stage_rob.io.fu_port.div_0 <> mul_div.io.div_req
  issue_stage_rob.io.fu_port.csr_0 <> csr.io.csr_req

  // csr <> csr_regs
  csr.io.csr_read_port <> csr_regs.io.read_port
  csr.io.csr_write_port <> csr_regs.io.write_port

  // issue stage <> reg file
  issue_stage_rob.io.gpr_read_port <> reg_file.io.read_ports

  // rob <> fu
  require(rob.io.fu_alu_wb_port.length == alu_seq.length)
  for (i <- alu_seq.indices) {
    rob.io.fu_alu_wb_port(i) <> alu_seq(i).io.out
  }
  rob.io.fu_lsu_wb_port <> lsu.io.lsu_resp
  rob.io.fu_lsu_agu_wb_port <> lsu.io.agu_writeback
  rob.io.fu_branch_wb_port <> bru.io.out
  rob.io.fu_mul_div_wb_port <> mul_div.io.fu_div_mul_resp
  rob.io.fu_csr_wb_port <> csr.io.csr_resp
  rob.io.fu_amo_wb_port <> lsu.io.amo_writeback

  // scoreboard <> commit stage
  rob.io.pop_ports <> commit_stage.io.rob_commit_ports

  // commit stage <> reg file
  commit_stage.io.gpr_commit_ports <> reg_file.io.write_ports
  // commit stage <> csr file
  commit_stage.io.direct_read_ports <> csr_regs.io.direct_read_ports
  commit_stage.io.direct_write_ports <> csr_regs.io.direct_write_ports
  commit_stage.io.cur_privilege_mode <> csr_regs.io.cur_privilege_mode

  // commit stage <> fu
  commit_stage.io.store_commit <> lsu.io.store_commit
  commit_stage.io.mmio_commit <> lsu.io.mmio_commit
  commit_stage.io.amo_commit <> lsu.io.amo_commit
  commit_stage.io.csr_commit <> csr.io.csr_commit

  // commit stage <> pc gen
  pc_gen_stage.io.redirect_pc <> commit_stage.io.branch_commit

  // commit stage <> monitor
  commit_stage.io.commit_monitor.get <> monitor.io.commit_monitor

  // regfile <> monitor
  reg_file.io.gpr_monitor.get <> monitor.io.gpr_monitor

  // csr <> monitor
  csr_regs.io.direct_read_ports <> monitor.io.csr_monitor

  // lsu <> tlb
  lsu.io.tlb_req <> lsu_tlb.io.tlb_req
  lsu.io.tlb_resp <> lsu_tlb.io.tlb_resp

  // lsu <> dcache
  lsu.io.dcache_load_req <> dcache.io.load_req
  lsu.io.dcache_store_req <> dcache.io.store_req
  lsu.io.dcache_load_resp <> dcache.io.load_resp
  lsu.io.dcache_store_resp <> dcache.io.store_resp

}

object gen_CoreTestFull extends App {
  GenVerilogHelper(
    new CoreDutFull(
      random_latency = false,
      muldiv_en = true,
      rvc_en = true
    ),
    "/home/leesum/workhome/chisel-fish/sim/vsrc/ysyx_v2.sv"
  )
}

/** This is a simplified version of CoreDutFull, which is used for VIVADO STA.
  */
object gen_CoreTestSTA extends App {
  GenVerilogHelper(
    new CoreDutFull(
      random_latency = true,
      muldiv_en = false
    ),
    "/home/leesum/vivado_project/ysyx_v2/ysyx_v2.srcs/sources_1/imports/ysyx_v2/ysyx_v2.sv"
  )
}
