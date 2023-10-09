package leesum
import chisel3._
import leesum.axi4.{AXI4Memory, AxiReadArbiter}
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class CoreTestDut(memFile: String) extends Module {

  // pipeline stage
  val pc_gen_stage = Module(new PCGenStage(0x80000000L))
  val fetch_stage = Module(new FetchStage)
  val inst_realign = Module(new InstReAlign)
  val inst_fifo = Module(new InstsFIFO)

  val decode_stage = Module(new InstDecoder)
  val decode_stage1 = Module(new InstDecoder)
  val issue_stage = Module(new IssueStage(2, 2))
  val score_board = Module(new ScoreBoard(8, 2, 2))
  val commit_stage = Module(new CommitStage(2))
  val reg_file = Module(new GPRs(2, 2))

  val fetch_tlb = Module(new DummyTLB(random_latency = false))
  val lsu_tlb = Module(new DummyTLB(random_latency = false))

  val dcache = Module(new DummyDCache)
  val icache = Module(new DummyICache)
  val axi_r_arb = Module(new AxiReadArbiter)
  val axi_mem = Module(
    new AXI4Memory(
      AXI_AW = 32,
      AXI_DW = 64,
      INTERNAL_MEM_SIZE = 0x100000,
      INTERNAL_MEM_DW = 64,
      INTERNAL_MEM_BASE = 0x80000000L,
      memoryFile = memFile
    )
  )

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

  axi_mem.io.ar <> axi_r_arb.io.out.ar
  axi_mem.io.r <> axi_r_arb.io.out.r
  axi_mem.io.aw <> dcache.io.axi_mem.aw
  axi_mem.io.w <> dcache.io.axi_mem.w
  axi_mem.io.b <> dcache.io.axi_mem.b

  // fu
  val alu = Module(new FuAlu)
  val lsu = Module(new LSU)
  val bru = Module(new FuBranch)

  // flush
  fetch_stage.io.flush := commit_stage.io.flush
  inst_fifo.io.flush := commit_stage.io.flush
  issue_stage.io.flush := commit_stage.io.flush
  score_board.io.flush := commit_stage.io.flush
  lsu.io.flush := commit_stage.io.flush
  fetch_tlb.io.flush := commit_stage.io.flush
  lsu_tlb.io.flush := commit_stage.io.flush
  dcache.io.flush := commit_stage.io.flush
  icache.io.flush := commit_stage.io.flush

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
  decode_stage.io.out <> issue_stage.io.push_port(0)
  decode_stage1.io.out <> issue_stage.io.push_port(1)

  // issue stage <> scoreboard
  issue_stage.io.pop_port <> score_board.io.push_ports
  issue_stage.io.new_trans_id <> score_board.io.push_trans_id
  issue_stage.io.rd_occupied_gpr <> score_board.io.rd_occupied_gpr
  issue_stage.io.scb_bypass <> score_board.io.operand_bypass

  // issue stage <> fu
  issue_stage.io.fu_port.alu_0 <> alu.io.in
  issue_stage.io.fu_port.lsu_0 <> lsu.io.lsu_req
  issue_stage.io.fu_port.branch_0 <> bru.io.in

  issue_stage.io.fu_port.csr_0.ready := false.B
  issue_stage.io.fu_port.mul_0.ready := false.B
  issue_stage.io.fu_port.div_0.ready := false.B
  // issue stage <> reg file
  issue_stage.io.gpr_read_port <> reg_file.io.read_ports

  // scoreboard <> fu
  score_board.io.fu_alu_wb_port <> alu.io.out
  score_board.io.fu_lsu_wb_port <> lsu.io.lsu_resp
  score_board.io.fu_branch_wb_port <> bru.io.out

  score_board.io.fu_mul_div_valid := false.B
  score_board.io.fu_mul_div_id := 0.U
  score_board.io.fu_mul_div_wb := 0.U
  score_board.io.fu_mul_div_wb_valid := false.B

  // scoreboard <> commit stage
  score_board.io.pop_ports <> commit_stage.io.rob_commit_ports

  // commit stage <> reg file
  commit_stage.io.gpr_commit_ports <> reg_file.io.write_ports

  // commit stage <> fu
  commit_stage.io.store_commit <> lsu.io.store_commit
  commit_stage.io.mmio_commit <> lsu.io.mmio_commit

  // commit stage <> pc gen
  pc_gen_stage.io.redirect_pc <> commit_stage.io.branch_commit

  // lsu <> tlb
  lsu.io.tlb_req <> lsu_tlb.io.tlb_req
  lsu.io.tlb_resp <> lsu_tlb.io.tlb_resp

  // lsu <> dcache
  lsu.io.dcache_load_req <> dcache.io.load_req
  lsu.io.dcache_store_req <> dcache.io.store_req
  lsu.io.dcache_load_resp <> dcache.io.load_resp
  lsu.io.dcache_store_resp <> dcache.io.store_resp

}

object gen_CoreTestDut_verilog extends App {
//  GenVerilogHelper(new CoreTestDut("src/main/resources/random_file.bin"))
  GenVerilogHelper(new CoreTestDut(""))
}

class CoreTest extends AnyFreeSpec with ChiselScalatestTester {

  val dummy_bin =
    "/home/leesum/workhome/ysyx/am-kernels/tests/cpu-tests/build/dummy-riscv64-nemu.bin"

  val add_bin =
    "/home/leesum/workhome/ysyx/am-kernels/tests/cpu-tests/build/add-riscv64-nemu.bin"

  val switch_bin =
    "/home/leesum/workhome/ysyx/am-kernels/tests/cpu-tests/build/switch-riscv64-nemu.bin"

//  "CoreTest1" in {
//    test(new CoreTestDut(switch_bin))
//      .withAnnotations(
//        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//      ) { dut =>
//        while (true) {
//          dut.clock.step(1)
//        }
//      }
//  }

}
