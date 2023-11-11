//package leesum
//import chisel3._
//import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
//import chisel3.experimental.VecLiterals._
//import chiseltest._
//import leesum.TestUtils.{int2UInt32, long2UInt64}
//import org.scalacheck.Gen
//import org.scalatest.freespec.AnyFreeSpec
//
//class IssueStageTest extends AnyFreeSpec with ChiselScalatestTester {
//
//  def gen_AluAdd_req(op_a: Long, op_b: Long, trans_id: Int) = {
//    (new AluReq).Lit(
//      _.trans_id -> int2UInt32(trans_id),
//      _.a -> long2UInt64(op_a),
//      _.b -> long2UInt64(op_b),
//      _.op -> AluOP.Add,
//      _.is_rv32 -> false.B
//    )
//  }
//
//  def default_reg_file(idx: Int) = {
//    idx
//  }
//
//  def gen_rd_occupied_gpr() = {
//    Vec(32, FuType()).Lit(0.until(32).map((_, FuType.None)): _*)
//  }
//
//  def gen_trans_id(id0: Int, id1: Int) = {
//    Vec(2, UInt(32.W)).Lit(0 -> id0.U, 1 -> id1.U)
//  }
//
//  def gen_add_scb_entry(
//      rd_addr: Int,
//      rs1_addr: Int,
//      rs2_addr: Int
//  ) = {
//    gen_scb_entry(
//      rd_addr,
//      rs1_addr,
//      rs2_addr,
//      use_pc = false,
//      FuType.Alu,
//      FuOP.AluAdd,
//      result = 0,
//      use_imm = false,
//      use_immz = false
//    )
//  }
//
//  def gen_lb_scb_entry(
//      rd_addr: Int,
//      rs1_addr: Int,
//      imm: Int
//  ) = {
//    gen_scb_entry(
//      rd_addr,
//      rs1_addr,
//      0,
//      use_pc = false,
//      FuType.Lsu,
//      FuOP.LsuLb,
//      result = imm,
//      use_imm = true,
//      use_immz = false
//    )
//  }
//
//  def gen_scb_entry(
//      rd_addr: Int,
//      rs1_addr: Int,
//      rs2_addr: Int,
//      use_pc: Boolean,
//      fu_type: FuType.Type,
//      fu_op: FuOP.Type,
//      result: Int,
//      use_imm: Boolean,
//      use_immz: Boolean
//  ) = {
//
//    (new ScoreBoardEntry).Lit(
//      _.complete -> false.B,
//      _.pc -> 0.U,
//      _.inst -> 0.U,
//      _.is_rvc -> false.B,
//      _.is_rv32 -> false.B,
//      _.fu_type -> fu_type,
//      _.fu_op -> fu_op,
//      _.rs1_addr -> rs1_addr.U,
//      _.rs2_addr -> rs2_addr.U,
//      _.rd_addr -> rd_addr.U,
//      _.result -> result.U,
//      _.result_valid -> false.B,
//      _.use_imm -> use_imm.B,
//      _.use_immz -> use_immz.B,
//      _.use_pc -> use_pc.B,
//      _.lsu_io_space -> false.B,
//      _.exception.valid -> false.B,
//      _.exception.tval -> 0.U,
//      _.exception.cause -> ExceptionCause.misaligned_fetch,
//      _.bp.is_taken -> false.B,
//      _.bp.predict_pc -> 0.U,
//      _.bp.is_miss_predict -> false.B,
//      _.bp.bp_type -> BpType.None
//    )
//  }
//
//  "IssueStage_BasicPushPop" in {
//    test(new IssueStage(2, 2))
//      .withAnnotations(
//        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//      ) { dut =>
//        init_port(dut)
//
//        val inst_seq = Gen
//          .listOfN(200, Gen.prob(0.5))
//          .sample
//          .get
//          .map(cond => {
//            if (cond) {
//              gen_add_scb_entry(
//                rd_addr = Gen.choose(0, 31).sample.get,
//                rs1_addr = Gen.choose(0, 31).sample.get,
//                rs2_addr = Gen.choose(0, 31).sample.get
//              )
//            } else {
//              gen_lb_scb_entry(
//                rd_addr = Gen.choose(0, 31).sample.get,
//                rs1_addr = Gen.choose(0, 31).sample.get,
//                imm = 0
//              )
//            }
//          })
//        val out_put_seq = inst_seq
//
//        // ----------------------
//        // push:1 pop:1
//        // ----------------------
//        dut.io.fu_port.alu_0.ready.poke(true.B)
//        dut.io.fu_port.lsu_0.ready.poke(true.B)
//        fork {
//          dut.io.push_port(0).enqueueSeq(inst_seq)
//        }.fork {
//          dut.io.pop_port(0).expectDequeueSeq(out_put_seq)
//        }.joinAndStep(dut.clock)
//        dut.clock.step(10)
//
//      }
//  }
//
//  "IssueStage_Between_Issue_hazard" in {
//    test(new IssueStage(2, 2))
//      .withAnnotations(
//        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//      ) { dut =>
//        init_port(dut)
//        val waw_seq = Seq(gen_add_scb_entry(1, 2, 3), gen_lb_scb_entry(1, 4, 0))
//        val raw_seq = Seq(gen_add_scb_entry(1, 2, 3), gen_lb_scb_entry(2, 1, 5))
//        val fu_hazard_seq = Seq(
//          gen_add_scb_entry(1, 2, 3),
//          gen_add_scb_entry(2, 1, 3)
//        )
//
//        dut.io.fu_port.alu_0.ready.poke(true.B)
//        dut.io.fu_port.lsu_0.ready.poke(true.B)
//        dut.io.pop_port(1).ready.poke(true.B)
//
//        // ----------------------
//        // waw between issue
//        // ----------------------
//        dut.io.pop_port(1).ready.poke(true.B)
//        dut.clock.step(5)
//        fork {
//          dut.io.push_port(0).enqueue(waw_seq(0))
//        }.fork {
//          dut.io.push_port(1).enqueue(waw_seq(1))
//        }.fork {
//          dut.clock.step(5)
//          dut.io.pop_port(0).expectDequeueSeq(waw_seq)
//        }.joinAndStep(dut.clock)
//        // ----------------------
//        // raw between issue
//        // ----------------------
//        dut.io.pop_port(1).ready.poke(true.B)
//        dut.clock.step(5)
//        fork {
//          dut.io.push_port(0).enqueue(raw_seq(0))
//        }.fork {
//          dut.io.push_port(1).enqueue(raw_seq(1))
//        }.fork {
//          dut.clock.step(5)
//          dut.io.pop_port(0).expectDequeueSeq(raw_seq)
//        }.joinAndStep(dut.clock)
//        // ----------------------
//        // fu hazard between issue
//        // ----------------------
//        dut.io.pop_port(1).ready.poke(true.B)
//        dut.clock.step(5)
//        fork {
//          dut.io.push_port(0).enqueue(fu_hazard_seq(0))
//        }.fork {
//          dut.io.push_port(1).enqueue(fu_hazard_seq(1))
//        }.fork {
//          dut.clock.step(5)
//          dut.io.pop_port(0).expectDequeueSeq(fu_hazard_seq)
//        }.joinAndStep(dut.clock)
//
//      }
//  }
//
//  "IssueStage_scb_hazard" in {
//    test(new IssueStage(2, 2))
//      .withAnnotations(
//        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//      ) { dut =>
//        init_port(dut)
//
//        // x2 is occupied by LSU
//        val fu_type_seq =
//          Seq.fill(32)(FuType.None).zipWithIndex.map { case (t, i) =>
//            if (i == 2) i -> FuType.Lsu else i -> t
//          }
//
//        val rd_occupied_gpr =
//          Vec(32, FuType()).Lit(
//            fu_type_seq: _*
//          )
//
//        val empty_rd_occupied = gen_rd_occupied_gpr()
//
//        val waw_seq =
//          Seq(gen_add_scb_entry(2, 3, 4), gen_lb_scb_entry(1, 4, 0))
//        val raw_seq = Seq(gen_add_scb_entry(1, 2, 3), gen_lb_scb_entry(4, 3, 5))
//
//        // ----------------------
//        // waw hazard
//        // ----------------------
//        dut.io.fu_port.alu_0.ready.poke(true.B)
//        dut.io.fu_port.lsu_0.ready.poke(true.B)
//        dut.io.rd_occupied_gpr.poke(rd_occupied_gpr)
//        dut.clock.step(1)
//        // push waw_seq
//        fork {
//          dut.io.push_port(0).enqueue(waw_seq(0))
//        }.fork {
//          dut.io.push_port(1).enqueue(waw_seq(1))
//        }.fork {
//          dut.clock.step(5)
//          // pop waw_seq
//          fork {
//            dut.io.pop_port(0).expectDequeue(waw_seq(0))
//          }.fork {
//            dut.io.pop_port(1).expectDequeue(waw_seq(1))
//          }.fork {
//            // because x2 is occupied by LSU, so issue stage should stall(pop will stall)
//            dut.clock.step(50)
//            // set x2 free, so issue stage can pop
//            dut.io.rd_occupied_gpr.poke(empty_rd_occupied)
//            dut.clock.step(1)
//          }.joinAndStep(dut.clock)
//        }.joinAndStep(dut.clock)
//
//        // ----------------------
//        // raw hazard
//        // ----------------------
//        dut.io.fu_port.alu_0.ready.poke(true.B)
//        dut.io.fu_port.lsu_0.ready.poke(true.B)
//        dut.io.rd_occupied_gpr.poke(rd_occupied_gpr)
//        dut.clock.step(1)
//        // push waw_seq
//        fork {
//          dut.io.push_port(0).enqueue(raw_seq(0))
//        }.fork {
//          dut.io.push_port(1).enqueue(raw_seq(1))
//        }.fork {
//          dut.clock.step(5)
//          // pop waw_seq
//          fork {
//            dut.io.pop_port(0).expectDequeue(raw_seq(0))
//          }.fork {
//            dut.io.pop_port(1).expectDequeue(raw_seq(1))
//          }.fork {
//            // because x2 is occupied by LSU, so issue stage should stall(pop will stall)
//            dut.clock.step(50)
//            // set x2 free, so issue stage can pop
//            dut.io.rd_occupied_gpr.poke(empty_rd_occupied)
//            dut.clock.step(1)
//          }.joinAndStep(dut.clock)
//        }.joinAndStep(dut.clock)
//
//      }
//  }
//
//  "IssueStage_bypass" in {
//    test(new IssueStage(2, 2))
//      .withAnnotations(
//        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//      ) { dut =>
//        init_port(dut)
//
//        // x2 is occupied by LSU
//        // x10 is occupied by ALU
//        val fu_type_seq =
//          Seq.fill(32)(FuType.None).zipWithIndex.map { case (t, i) =>
//            if (i == 2) i -> FuType.Lsu
//            else if (i == 10) i -> FuType.Alu
//            else i -> t
//          }
//
//        val rd_occupied_gpr =
//          Vec(32, FuType()).Lit(
//            fu_type_seq: _*
//          )
//        val bypass_seq = Gen.listOfN(32, Gen.long).sample.get
//
//        val empty_rd_occupied = gen_rd_occupied_gpr()
//
//        val add_seq =
//          Seq(
//            gen_add_scb_entry(3, 2, 4),
//            gen_add_scb_entry(4, 1, 2),
//            gen_add_scb_entry(5, 10, 2)
//          )
//
//        val add_resp = Seq(
//          gen_AluAdd_req(bypass_seq(2), 0, 0),
//          gen_AluAdd_req(0, bypass_seq(2), 0),
//          gen_AluAdd_req(bypass_seq(10), bypass_seq(2), 0)
//        )
//
//        // ----------------------
//        // bypass test
//        // ----------------------
//        dut.io.rd_occupied_gpr.poke(rd_occupied_gpr)
//        dut.clock.step(1)
//        fork {
//          dut.io.push_port(0).enqueueSeq(add_seq)
//        }.fork {
//          var count = 100
//          while (count > 0) {
//            val rs1_addr = dut.io.scb_bypass(0).rs1_addr.peek().litValue.toInt
//            val rs2_addr = dut.io.scb_bypass(0).rs2_addr.peek().litValue.toInt
//            timescope {
//              if (fu_type_seq(rs1_addr)._2 != FuType.None) {
//                dut.io.scb_bypass(0).rs1_fwd_valid.poke(true.B)
//                dut.io
//                  .scb_bypass(0)
//                  .rs1_data
//                  .poke(long2UInt64(bypass_seq(rs1_addr)))
//              }
//              if (fu_type_seq(rs2_addr)._2 != FuType.None) {
//                dut.io.scb_bypass(0).rs2_fwd_valid.poke(true.B)
//                dut.io
//                  .scb_bypass(0)
//                  .rs2_data
//                  .poke(long2UInt64(bypass_seq(rs2_addr)))
//              }
//              dut.clock.step()
//            }
//            count -= 1
//          }
//        }.fork {
//          dut.io.fu_port.alu_0.expectDequeueSeq(add_resp)
//        }.fork {
//          dut.io.pop_port(0).expectDequeueSeq(add_seq)
//        }.joinAndStep(dut.clock)
//
//      }
//  }
//
//  "IssueStage_flush" in {
//    test(new IssueStage(2, 2))
//      .withAnnotations(
//        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//      ) { dut =>
//        init_port(dut)
//
//        def _gen_scb(cond: Boolean) = {
//          if (cond) {
//            gen_add_scb_entry(
//              rd_addr = Gen.choose(0, 31).sample.get,
//              rs1_addr = Gen.choose(0, 31).sample.get,
//              rs2_addr = Gen.choose(0, 31).sample.get
//            )
//          } else {
//            gen_lb_scb_entry(
//              rd_addr = Gen.choose(0, 31).sample.get,
//              rs1_addr = Gen.choose(0, 31).sample.get,
//              imm = 0
//            )
//          }
//        }
//
//        val inst_seq_before_flush = Gen
//          .listOfN(4, Gen.prob(0.5))
//          .sample
//          .get
//          .map(cond => {
//            _gen_scb(cond)
//          })
//
//        val inst_seq_after_flush = Gen
//          .listOfN(1000, Gen.prob(0.5))
//          .sample
//          .get
//          .map(cond => {
//            _gen_scb(cond)
//          })
//
//        val out_seq_after_flush = inst_seq_after_flush
//
//        // ----------------------
//        // flush
//        // ----------------------
//        dut.io.push_port(0).enqueueSeq(inst_seq_before_flush)
//        timescope {
//          dut.io.flush.poke(true.B)
//          dut.clock.step(1)
//        }
//
//        // ----------------------
//        // push:1 pop:1
//        // ----------------------
//        dut.io.fu_port.alu_0.ready.poke(true.B)
//        dut.io.fu_port.lsu_0.ready.poke(true.B)
//        fork {
//          dut.io.push_port(0).enqueueSeq(inst_seq_after_flush)
//        }.fork {
//          dut.io.pop_port(0).expectDequeueSeq(out_seq_after_flush)
//        }.joinAndStep(dut.clock)
//        dut.clock.step(10)
//
//      }
//  }
//
//  private def init_port(dut: IssueStage): Unit = {
//    dut.io.push_port.foreach(_.initSource().setSourceClock(dut.clock))
//    dut.io.pop_port.foreach(_.initSink().setSinkClock(dut.clock))
//    dut.io.fu_port.alu_0.initSink().setSinkClock(dut.clock)
//    dut.io.fu_port.lsu_0.initSink().setSinkClock(dut.clock)
//    dut.io.fu_port.branch_0.initSink().setSinkClock(dut.clock)
//    dut.io.fu_port.mul_0.initSink().setSinkClock(dut.clock)
//    dut.io.fu_port.csr_0.initSink().setSinkClock(dut.clock)
//    dut.io.fu_port.div_0.initSink().setSourceClock(dut.clock)
//    dut.io.flush.poke(false.B)
//
//    val empty_rd_occupied = gen_rd_occupied_gpr()
//    dut.io.rd_occupied_gpr.poke(empty_rd_occupied)
//    dut.io.gpr_read_port.map(port => {
//      port.rs1_data.poke(0.U)
//      port.rs2_data.poke(0.U)
//    })
//
//    dut.io.scb_bypass.map(bypass => {
//      bypass.rs1_fwd_valid.poke(false.B)
//      bypass.rs2_fwd_valid.poke(false.B)
//      bypass.rs1_data.poke(0.U)
//      bypass.rs2_data.poke(0.U)
//    })
//
//    val empty_id = gen_trans_id(0, 0)
//    dut.io.new_trans_id.poke(empty_id)
//    dut.clock.step(5)
//  }
//}
