package leesum.Core

import chisel3._
import chisel3.util.{Valid, log2Ceil}
import leesum.axi4.{AXI4SlaveBrige, AxiReadArbiter, BasicMemoryIO}
import leesum.moniter.{DifftestPort, MonitorTop}
import leesum._
class CoreDutVivadoSTA(random_latency: Boolean = false) extends Module {
  val base_addr = 0x80000000L
  val mem_size = 0x100000L
  val monitor_en = true
  val addr_width = log2Ceil(0x80000000L + 0x100000L)

  val io = IO(new Bundle {
    val difftest = Output(Valid(new DifftestPort))
    val mem_port = Flipped(new BasicMemoryIO(addr_width, 64))
  })

  // monitor
  val monitor = Module(new MonitorTop(2))
  io.difftest <> monitor.io.difftest

  // pipeline stage
  val pc_gen_stage = Module(new PCGenStage(0x80000000L))
  val fetch_stage = Module(new FetchStage)
  val inst_realign = Module(new InstReAlign)
  val inst_fifo = Module(new InstsFIFO)

  val decode_stage = Module(new InstDecoder)
  val decode_stage1 = Module(new InstDecoder)

  val rob = Module(new ReOrderBuffer(8, 2, 2))
  val issue_stage_rob = Module(new IssueStageNew(2, 2))

  val commit_stage = Module(new CommitStage(2, monitor_en))
  val reg_file = Module(new GPRs(2, 2, monitor_en))

  val fetch_tlb = Module(new DummyTLB(random_latency))
  val lsu_tlb = Module(new DummyTLB(random_latency))

  val dcache = Module(new DummyDCache)
  val icache = Module(new DummyICache)
  val axi_r_arb = Module(new AxiReadArbiter)
  val axi_mem = Module(
    new AXI4SlaveBrige(
      AXI_AW = 32,
      AXI_DW = 64,
      INTERNAL_MEM_SIZE = 0x100000,
      INTERNAL_MEM_DW = 64,
      INTERNAL_MEM_BASE = 0x80000000L
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

  // fu
  val alu = Module(new FuAlu)
  val lsu = Module(new LSU)
  val bru = Module(new FuBranch)
//  val mul_div = Module(new FuMulDiv)

  // flush
  fetch_stage.io.flush := commit_stage.io.flush
  inst_fifo.io.flush := commit_stage.io.flush
  issue_stage_rob.io.flush := commit_stage.io.flush
  rob.io.flush := commit_stage.io.flush
  lsu.io.flush := commit_stage.io.flush
  fetch_tlb.io.flush := commit_stage.io.flush
  lsu_tlb.io.flush := commit_stage.io.flush
  dcache.io.flush := commit_stage.io.flush
  icache.io.flush := commit_stage.io.flush
  inst_realign.flush := commit_stage.io.flush
  bru.io.flush := commit_stage.io.flush
//  mul_div.io.flush := commit_stage.io.flush

  // pc_gen_stage <> fetch_stage
  pc_gen_stage.io.pc <> fetch_stage.io.pc_in

  // fetch_stage <> inst_realign
  inst_realign.input.valid := fetch_stage.io.fetch_resp.valid
  fetch_stage.io.fetch_resp.ready := inst_realign.input.ready
  inst_realign.input.bits.pc := fetch_stage.io.fetch_resp.bits.pc
  inst_realign.input.bits.payload := fetch_stage.io.fetch_resp.bits.data

  // fetch_stage <> tlb_arb
  fetch_tlb.io.tlb_req <> fetch_stage.io.tlb_req
  fetch_tlb.io.tlb_resp <> fetch_stage.io.tlb_resp

  // fetch_stage <> icache
  fetch_stage.io.icache_req <> icache.io.load_req
  fetch_stage.io.icache_resp <> icache.io.load_resp

  // inst_realign <> inst_fifo
  inst_fifo.io.push <> inst_realign.output

  // inst_fifo <> decode stage
  inst_fifo.io.pop(0).ready := decode_stage.io.in.ready
  decode_stage.io.in.valid := inst_fifo.io.pop(0).valid
  decode_stage.io.in.bits.inst := inst_fifo.io.pop(0).bits.inst
  decode_stage.io.in.bits.pc := inst_fifo.io.pop(0).bits.pc
  decode_stage.io.in.bits.is_rvc := inst_fifo.io.pop(0).bits.rvc
  decode_stage.io.in.bits.is_valid := inst_fifo.io.pop(0).bits.valid
  decode_stage.io.in.bits.exception := DontCare
  decode_stage.io.in.bits.bp := DontCare
  decode_stage.io.in.bits.exception.valid := false.B
  decode_stage.io.in.bits.bp.is_taken := false.B

  inst_fifo.io.pop(1).ready := decode_stage1.io.in.ready
  decode_stage1.io.in.valid := inst_fifo.io.pop(1).valid
  decode_stage1.io.in.bits.inst := inst_fifo.io.pop(1).bits.inst
  decode_stage1.io.in.bits.pc := inst_fifo.io.pop(1).bits.pc
  decode_stage1.io.in.bits.is_rvc := inst_fifo.io.pop(1).bits.rvc
  decode_stage1.io.in.bits.is_valid := inst_fifo.io.pop(1).bits.valid
  decode_stage1.io.in.bits.exception := DontCare
  decode_stage1.io.in.bits.bp := DontCare
  decode_stage1.io.in.bits.exception.valid := false.B
  decode_stage1.io.in.bits.bp.is_taken := false.B

  // decode stage <> issue stage
  decode_stage.io.out <> issue_stage_rob.io.push_port(0)
  decode_stage1.io.out <> issue_stage_rob.io.push_port(1)

  // issue stage <> scoreboard
  issue_stage_rob.io.pop_port <> rob.io.push_ports
  issue_stage_rob.io.alloc_trans_id <> rob.io.push_trans_id
  issue_stage_rob.io.operand_bypass_resp := rob.io.operand_bypass_resp
  rob.io.operand_bypass_req := issue_stage_rob.io.operand_bypass_req

  // issue stage <> fu
  issue_stage_rob.io.fu_port.alu_0 <> alu.io.in
  issue_stage_rob.io.fu_port.lsu_0 <> lsu.io.lsu_req
  issue_stage_rob.io.fu_port.branch_0 <> bru.io.in
  issue_stage_rob.io.fu_port.mul_0.nodeq()
  issue_stage_rob.io.fu_port.div_0.nodeq()

  // TODO: CSR NOT IMPLEMENTED
  issue_stage_rob.io.fu_port.csr_0.ready := false.B
  // issue stage <> reg file
  issue_stage_rob.io.gpr_read_port <> reg_file.io.read_ports

  // scoreboard <> fu
  rob.io.fu_alu_wb_port <> alu.io.out
  rob.io.fu_lsu_wb_port <> lsu.io.lsu_resp
  rob.io.fu_branch_wb_port <> bru.io.out

  rob.io.fu_mul_div_wb_port.noenq()

  // scoreboard <> commit stage
  rob.io.pop_ports <> commit_stage.io.rob_commit_ports

  // commit stage <> reg file
  commit_stage.io.gpr_commit_ports <> reg_file.io.write_ports

  // commit stage <> fu
  commit_stage.io.store_commit <> lsu.io.store_commit
  commit_stage.io.mmio_commit <> lsu.io.mmio_commit

  // commit stage <> pc gen
  pc_gen_stage.io.redirect_pc <> commit_stage.io.branch_commit

  // commit stage <> monitor
  commit_stage.io.commit_monitor.get <> monitor.io.commit_monitor

  // regfile <> monitor
  reg_file.io.gpr_monitor.get <> monitor.io.gpr_monitor

  // lsu <> tlb
  lsu.io.tlb_req <> lsu_tlb.io.tlb_req
  lsu.io.tlb_resp <> lsu_tlb.io.tlb_resp

  // lsu <> dcache
  lsu.io.dcache_load_req <> dcache.io.load_req
  lsu.io.dcache_store_req <> dcache.io.store_req
  lsu.io.dcache_load_resp <> dcache.io.load_resp
  lsu.io.dcache_store_resp <> dcache.io.store_resp

}

object gen_CoreTestDutVivadoSTA_verilog extends App {
  GenVerilogHelper(new CoreDutVivadoSTA())
}
