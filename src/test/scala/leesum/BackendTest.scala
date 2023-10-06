package leesum

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.Decoupled
import chiseltest._
import leesum.TestUtils.{
  check_aligned,
  gen_axi_wdata,
  gen_axi_wstrb,
  gen_rand_uint,
  long2UInt64
}
import leesum.axi4.AXI4Memory
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class BackendTestDut extends Module {
  val io = IO(new Bundle {
    val inst_fetch = Flipped(Decoupled(new FetchEntry()))
    val inst_fetch1 = Flipped(Decoupled(new FetchEntry()))
  })

  // pipeline stage
  val decode_stage = Module(new InstDecoder)
  val decode_stage1 = Module(new InstDecoder)
  val issue_stage = Module(new IssueStage(2, 2))
  val score_board = Module(new ScoreBoard(8, 2, 2))
  val commit_stage = Module(new CommitStage(2))
  val reg_file = Module(new GPRs(2, 2))

  val tlb = Module(new DummyTLB)
  val dcache = Module(new DummyDCache)
  val axi_mem = Module(
    new AXI4Memory(
      AXI_AW = 32,
      AXI_DW = 64,
      INTERNAL_MEM_SIZE = 2048,
      INTERNAL_MEM_DW = 64,
      INTERNAL_MEM_BASE = 0,
      memoryFile = ""
    )
  )

  // fu
  val alu = Module(new FuAlu)
  val lsu = Module(new LSU)
  val bru = Module(new FuBranch)

  // flush
  issue_stage.io.flush := commit_stage.io.flush
  score_board.io.flush := commit_stage.io.flush
  lsu.io.flush := commit_stage.io.flush
  tlb.io.flush := commit_stage.io.flush
  dcache.io.flush := commit_stage.io.flush

  // dcache <> axi_mem
  axi_mem.io <> dcache.io.axi_mem

  // lsu <> tlb
  lsu.io.tlb_req <> tlb.io.tlb_req
  lsu.io.tlb_resp <> tlb.io.tlb_resp

  // lsu <> dcache
  lsu.io.dcache_load_req <> dcache.io.load_req
  lsu.io.dcache_store_req <> dcache.io.store_req
  lsu.io.dcache_load_resp <> dcache.io.load_resp
  lsu.io.dcache_store_resp <> dcache.io.store_resp

  // decode stage <> TestDut in
  decode_stage.io.in <> io.inst_fetch
  decode_stage1.io.in <> io.inst_fetch1
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

  commit_stage.io.branch_commit.ready := false.B

}

object gen_backendDut_verilog extends App {
  GenVerilogHelper(new BackendTestDut)
}

class BackendTest extends AnyFreeSpec with ChiselScalatestTester {}
