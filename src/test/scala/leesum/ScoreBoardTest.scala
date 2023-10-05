package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals.AddVecLiteralConstructor
import chiseltest._
import leesum.TestUtils.long2UInt64
import leesum.axi4.skid_buffer
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class ScoreBoardTest extends AnyFreeSpec with ChiselScalatestTester {

  def gen_add_scb_entry(
      rd_addr: Int,
      rs1_addr: Int,
      rs2_addr: Int,
      pc: Long
  ) = {
    gen_scb_entry(
      rd_addr,
      rs1_addr,
      rs2_addr,
      pc,
      FuType.Alu,
      FuOP.AluAdd,
      result = 0,
      use_pc = false,
      use_imm = false,
      use_immz = false,
      complete = false,
      result_valid = false
    )
  }
  def gen_scb_entry(
      rd_addr: Int,
      rs1_addr: Int,
      rs2_addr: Int,
      pc: Long,
      fu_type: FuType.Type,
      fu_op: FuOP.Type,
      result: Long,
      use_pc: Boolean,
      use_imm: Boolean,
      use_immz: Boolean,
      complete: Boolean,
      result_valid: Boolean
  ) = {
    (new ScoreBoardEntry).Lit(
      _.complete -> complete.B,
      _.pc -> long2UInt64(pc),
      _.inst -> 0.U,
      _.is_rvc -> false.B,
      _.is_rv32 -> false.B,
      _.fu_type -> fu_type,
      _.fu_op -> fu_op,
      _.rs1_addr -> rs1_addr.U,
      _.rs2_addr -> rs2_addr.U,
      _.rd_addr -> rd_addr.U,
      _.result -> result.U,
      _.result_valid -> result_valid.B,
      _.use_imm -> use_imm.B,
      _.use_immz -> use_immz.B,
      _.use_pc -> use_pc.B,
      _.lsu_io_space -> false.B,
      _.exception.valid -> false.B,
      _.exception.tval -> 0.U,
      _.exception.cause -> ExceptionCause.misaligned_fetch,
      _.bp.is_taken -> false.B,
      _.bp.predict_pc -> 0.U,
      _.bp.is_miss_predict -> false.B,
      _.bp.bp_type -> BpType.None
    )
  }

  "ScoreBoard_rd_occupied_gpr_test" in {
    test(new ScoreBoard(8, 2, 2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) {
        dut =>
          // ----------------
          // Init ports
          // ----------------
          init_port(dut)

          // ----------------
          // prepare test data
          // ----------------
          val add1_scb = gen_add_scb_entry(1, 2, 3, 0x1000)
          val add2_scb = gen_add_scb_entry(2, 3, 4, 0x1004)
          val add3_scb = gen_add_scb_entry(3, 4, 5, 0x1008)
          val add4_scb = gen_add_scb_entry(10, 4, 5, 0x100c)

          val fu_type_seq = Seq(
            Seq(1),
            Seq(1, 2),
            Seq(1, 2, 3),
            Seq(1, 2, 3, 10),
            Seq(2, 3, 10),
            Seq(3, 10),
            Seq(10),
            Seq()
          ).map { validIndices =>
            Seq.tabulate(32) { idx =>
              if (validIndices.contains(idx)) (idx, FuType.Alu)
              else (idx, FuType.None)
            }
          }

          val pushAndExpect =
            (
                scb: ScoreBoardEntry,
                expectedFuTypeSeq: Seq[(Int, FuType.Type)]
            ) => {
              dut.io.push_ports(0).enqueue(scb)
              dut.io.rd_occupied_gpr.expect(
                Vec(32, FuType()).Lit(expectedFuTypeSeq: _*)
              )
            }

          val popAndExpect =
            (
                scb: ScoreBoardEntry,
                expectedFuTypeSeq: Seq[(Int, FuType.Type)]
            ) => {
              dut.io.pop_ports(0).expectDequeue(scb)
              dut.io.rd_occupied_gpr.expect(
                Vec(32, FuType()).Lit(expectedFuTypeSeq: _*)
              )
            }

          // ---------------------
          // rd_occupied_gpr test
          // ---------------------
          pushAndExpect(add1_scb, fu_type_seq(0))
          pushAndExpect(add2_scb, fu_type_seq(1))
          pushAndExpect(add3_scb, fu_type_seq(2))
          pushAndExpect(add4_scb, fu_type_seq(3))

          popAndExpect(add1_scb, fu_type_seq(4))
          popAndExpect(add2_scb, fu_type_seq(5))
          popAndExpect(add3_scb, fu_type_seq(6))
          popAndExpect(add4_scb, fu_type_seq(7))

          dut.clock.step(5)
      }
  }

  "ScoreBoard_basic_push_pop_flush" in {
    test(new ScoreBoard(8, 2, 2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) {
        dut =>
          // ----------------
          // Init ports
          // ----------------
          init_port(dut)

          // ------------------
          // prepare test data
          // ------------------
          val push_seq = Seq.tabulate(32)(idx => {
            val addr_gen = Gen.chooseNum(0, 31)
            gen_add_scb_entry(
              idx,
              addr_gen.sample.get,
              addr_gen.sample.get,
              0x1000 + idx * 4
            )
          })
          val pop_seq = push_seq

          // -----------------------------
          // 1 push 1 push without bubble
          // -----------------------------
          fork {
            dut.io.push_ports(0).enqueueSeq(push_seq)
          }.fork {
            dut.io.pop_ports(0).expectDequeueSeq(pop_seq)
          }.joinAndStep(dut.clock)

          // -----------------------------
          // 1 push 1 push with bubble
          // -----------------------------
          fork {
            push_seq.foreach(data => {
              dut.clock.step(Gen.chooseNum(1, 10).sample.get)
              dut.io.push_ports(0).enqueue(data)
            })
          }.fork {
            pop_seq.foreach(data => {
              dut.clock.step(Gen.chooseNum(1, 10).sample.get)
              dut.io.pop_ports(0).expectDequeue(data)
            })
          }.joinAndStep(dut.clock)

          // -----------------------------
          // flush test
          // -----------------------------
          dut.io.push_ports(0).enqueueSeq(push_seq.take(5))

          timescope {
            dut.io.flush.poke(true.B)
            dut.clock.step(1)
          }

          fork {
            push_seq.foreach(data => {
              dut.clock.step(Gen.chooseNum(1, 10).sample.get)
              dut.io.push_ports(0).enqueue(data)
            })
          }.fork {
            pop_seq.foreach(data => {
              dut.clock.step(Gen.chooseNum(1, 10).sample.get)
              dut.io.pop_ports(0).expectDequeue(data)
            })
          }.joinAndStep(dut.clock)

      }
  }

  "ScoreBoard_rob_bypass_test" in {
    test(new ScoreBoard(8, 2, 2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) {
        dut =>
          // ----------------
          // Init ports
          // ----------------
          init_port(dut)

          // ------------------
          // prepare test data
          // ------------------

          val complete_scb_seq = Seq(
            gen_complete_add_scb_entry(1, 2, 3, 0x1000, 0x100),
            gen_complete_add_scb_entry(2, 3, 4, 0x1004, 0x200),
            gen_complete_add_scb_entry(3, 4, 5, 0x1008, 0x300),
            gen_complete_add_scb_entry(0, 5, 6, 0x100c, 0x400)
          )
          val uncomplete_scb_seq = Seq(
            gen_add_scb_entry(10, 2, 3, 0x2000),
            gen_add_scb_entry(11, 3, 4, 0x2004),
            gen_add_scb_entry(12, 4, 5, 0x2008),
            gen_add_scb_entry(13, 5, 6, 0x200c)
          )

          // -----------------------------
          // rob bypass_test
          // -----------------------------

          // push complete_scb_seq and uncompleted_scb_seq
          dut.io
            .push_ports(0)
            .enqueueSeq(complete_scb_seq ++ uncomplete_scb_seq)

          // bypass check
          // 1. rs1_addr = 1, rs2_addr = 2, rs1_fwd_valid = true, rs2_fwd_valid = true
          dut.io.operand_bypass(0).rs1_addr.poke(1.U)
          dut.io.operand_bypass(0).rs2_addr.poke(2.U)
          // 2. rs1_addr = 10, rs2_addr = 3, rs1_fwd_valid = false, rs2_fwd_valid = true
          dut.io.operand_bypass(1).rs1_addr.poke(10.U)
          dut.io.operand_bypass(1).rs2_addr.poke(3.U)

          dut.io.operand_bypass(0).rs1_fwd_valid.expect(true.B)
          dut.io.operand_bypass(0).rs2_fwd_valid.expect(true.B)
          dut.io.operand_bypass(0).rs1_data.expect(0x100.U)
          dut.io.operand_bypass(0).rs2_data.expect(0x200.U)

          dut.io.operand_bypass(1).rs1_fwd_valid.expect(false.B)
          dut.io.operand_bypass(1).rs2_fwd_valid.expect(true.B)
          dut.io.operand_bypass(1).rs2_data.expect(0x300.U)

          // pop complete_scb_seq, after that, x1 bypass should be invalid
          dut.io.pop_ports(0).expectDequeue(complete_scb_seq(0))

          // bypass check
          // 1. rs1_addr = 1, rs2_addr = 2, rs1_fwd_valid = false, rs2_fwd_valid = true
          dut.io.operand_bypass(0).rs1_addr.poke(1.U)
          dut.io.operand_bypass(0).rs2_addr.poke(2.U)
          // 2. rs1_addr = 10, rs2_addr = 3, rs1_fwd_valid = false, rs2_fwd_valid = true
          dut.io.operand_bypass(1).rs1_addr.poke(10.U)
          dut.io.operand_bypass(1).rs2_addr.poke(3.U)

          dut.io.operand_bypass(0).rs1_fwd_valid.expect(false.B)
          dut.io.operand_bypass(0).rs2_fwd_valid.expect(true.B)
          dut.io.operand_bypass(0).rs2_data.expect(0x200.U)
          dut.io.operand_bypass(1).rs1_fwd_valid.expect(false.B)
          dut.io.operand_bypass(1).rs2_fwd_valid.expect(true.B)
          dut.io.operand_bypass(1).rs2_data.expect(0x300.U)

          // pop complete_scb_seq, after that, x2 bypass should be invalid
          dut.io.pop_ports(0).expectDequeue(complete_scb_seq(1))

          // bypass check
          // 1. rs1_addr = 1, rs2_addr = 2, rs1_fwd_valid = false, rs2_fwd_valid = false
          dut.io.operand_bypass(0).rs1_addr.poke(1.U)
          dut.io.operand_bypass(0).rs2_addr.poke(2.U)
          // 2. rs1_addr = 10, rs2_addr = 3, rs1_fwd_valid = false, rs2_fwd_valid = true
          dut.io.operand_bypass(1).rs1_addr.poke(10.U)
          dut.io.operand_bypass(1).rs2_addr.poke(3.U)

          dut.io.operand_bypass(0).rs1_fwd_valid.expect(false.B)
          dut.io.operand_bypass(0).rs2_fwd_valid.expect(false.B)
          dut.io.operand_bypass(1).rs1_fwd_valid.expect(false.B)
          dut.io.operand_bypass(1).rs2_fwd_valid.expect(true.B)
          dut.io.operand_bypass(1).rs2_data.expect(0x300.U)

          dut.clock.step(5)

          // x0 should not be bypassed
          dut.io.operand_bypass(0).rs1_addr.poke(0.U)
          dut.io.operand_bypass(0).rs2_addr.poke(0.U)
          dut.io.operand_bypass(1).rs1_addr.poke(0.U)
          dut.io.operand_bypass(1).rs2_addr.poke(0.U)

          dut.io.operand_bypass(0).rs1_fwd_valid.expect(false.B)
          dut.io.operand_bypass(0).rs2_fwd_valid.expect(false.B)
          dut.io.operand_bypass(1).rs1_fwd_valid.expect(false.B)
          dut.io.operand_bypass(1).rs2_fwd_valid.expect(false.B)

          dut.clock.step(5)
      }
  }

  "ScoreBoard_write-back_bypass_test" in {
    test(new ScoreBoard(8, 2, 2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) {
        dut =>
          // ----------------
          // Init ports
          // ----------------
          init_port(dut)

          // ------------------
          // prepare test data
          // ------------------

          val complete_scb_seq = Seq(
            // id 0
            gen_complete_add_scb_entry(1, 2, 3, 0x1000, 0x100),
            // id 1
            gen_complete_add_scb_entry(2, 3, 4, 0x1004, 0x200),
            // id 2
            gen_complete_add_scb_entry(3, 5, 6, 0x100c, 0x400)
          )
          val uncomplete_scb_seq = Seq(
            // id 3
            gen_add_scb_entry(10, 2, 3, 0x2000),
            // id 4
            gen_add_scb_entry(11, 3, 4, 0x2004),
            // id 5
            gen_add_scb_entry(0, 4, 5, 0x2008)
          )
          // -----------------------------
          // write-back bypass_test
          // -----------------------------

          // push complete_scb_seq and uncompleted_scb_seq
          dut.io
            .push_ports(0)
            .enqueueSeq(complete_scb_seq ++ uncomplete_scb_seq)

          // bypass check
          // 1. rs1_addr = 10, rs2_addr = 11, rs1_fwd_valid = false, rs2_fwd_valid = false
          dut.io.operand_bypass(0).rs1_addr.poke(10.U)
          dut.io.operand_bypass(0).rs2_addr.poke(11.U)
          // 2. rs1_addr = 1, rs2_addr = 12, rs1_fwd_valid = true, rs2_fwd_valid = false
          dut.io.operand_bypass(1).rs1_addr.poke(1.U)
          dut.io.operand_bypass(1).rs2_addr.poke(12.U)

          dut.io.operand_bypass(0).rs1_fwd_valid.expect(false.B)
          dut.io.operand_bypass(0).rs2_fwd_valid.expect(false.B)

          dut.io.operand_bypass(1).rs1_fwd_valid.expect(true.B)
          dut.io.operand_bypass(1).rs2_fwd_valid.expect(false.B)
          dut.io.operand_bypass(1).rs1_data.expect(0x100.U)

          dut.clock.step(5)

          fork {
            // id 3, rd_addr = 10
            dut.io.fu_alu_wb_port.enqueue(
              gen_alu_resp(3, 0x1112)
            )
          }.fork
            .withRegion(Monitor) {
              // bypass check
              // 1. rs1_addr = 10, rs2_addr = 11, rs1_fwd_valid = true, rs2_fwd_valid = false
              dut.io.operand_bypass(0).rs1_addr.poke(10.U)
              dut.io.operand_bypass(0).rs2_addr.poke(11.U)

              dut.io.operand_bypass(0).rs1_fwd_valid.expect(true.B)
              dut.io.operand_bypass(0).rs2_fwd_valid.expect(false.B)
              dut.io.operand_bypass(0).rs1_data.expect(0x1112.U)
              dut.clock.step(5)
            }
            .joinAndStep(dut.clock)

          fork {
            // id 5, rd_addr = 0, x0 should not be bypassed
            dut.io.fu_alu_wb_port.enqueue(
              gen_alu_resp(5, 0x3333)
            )
          }.fork
            .withRegion(Monitor) {
              // bypass check
              dut.io.operand_bypass(0).rs1_addr.poke(0.U)
              dut.io.operand_bypass(0).rs2_addr.poke(0.U)

              dut.io.operand_bypass(0).rs1_fwd_valid.expect(false.B)
              dut.io.operand_bypass(0).rs2_fwd_valid.expect(false.B)
              dut.clock.step(5)
            }
            .joinAndStep(dut.clock)

      }
  }

  def gen_alu_resp(
      id: Int,
      result: Long
  ) = {
    (new AluResp).Lit(
      _.trans_id -> id.U,
      _.res -> long2UInt64(result)
    )
  }

  def gen_complete_add_scb_entry(
      rd_addr: Int,
      rs1_addr: Int,
      rs2_addr: Int,
      pc: Long,
      result: Long
  ) = {
    gen_scb_entry(
      rd_addr,
      rs1_addr,
      rs2_addr,
      pc,
      FuType.Alu,
      FuOP.AluAdd,
      result = result,
      use_pc = false,
      use_imm = false,
      use_immz = false,
      complete = true,
      result_valid = true
    )
  }

  private def init_port(dut: ScoreBoard): Unit = {
    dut.io.push_ports.foreach(_.initSource().setSourceClock(dut.clock))
    dut.io.pop_ports.foreach(_.initSink().setSinkClock(dut.clock))
    dut.io.fu_alu_wb_port.initSource().setSourceClock(dut.clock)
    dut.io.fu_branch_wb_port.initSource().setSourceClock(dut.clock)
    dut.io.fu_lsu_wb_port.initSource().setSourceClock(dut.clock)

    dut.io.fu_mul_div_valid.poke(false.B)
    dut.io.fu_mul_div_wb_valid.poke(false.B)
    dut.io.fu_mul_div_wb.poke(0.U)
    dut.io.fu_mul_div_id.poke(0.U)

    dut.io.operand_bypass.foreach(bypass => {
      bypass.rs1_addr.poke(0.U)
      bypass.rs2_addr.poke(0.U)
    })

    dut.io.flush.poke(false.B)
  }
}
