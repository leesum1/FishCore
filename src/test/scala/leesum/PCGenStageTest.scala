//package leesum
//import chisel3._
//import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
//import chisel3.util.Decoupled
//import chiseltest._
//import leesum.TestUtils.{check_aligned, int2UInt32, long2UInt64}
//import leesum.fronten.PCGenStage
//import org.scalacheck.Gen
//import org.scalatest.freespec.AnyFreeSpec
//
//class PCGenStageTest extends AnyFreeSpec with ChiselScalatestTester {
//  "PCGenStageTest1" in {
//    val boot_pc = 1000
//    test(new PCGenStage(boot_pc))
//      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) {
//        dut =>
//          // --------------
//          // init port
//          // --------------
//          init_port(dut)
//
//          // --------------------
//          // prepare test data
//          // --------------------
//          val expect_pc_seq = Seq.tabulate(100) { i =>
//            (boot_pc + i * 8).U
//          }
//
//          val (split_seq1, split_seq2) =
//            expect_pc_seq.splitAt(expect_pc_seq.size / 2)
//
//          val redirect_pc =
//            new RedirectPC().Lit(_.target -> 0x80000004L.U(64.W))
//
//          val after_redirect_pc = Seq.tabulate(100) { i =>
//            if (i == 0) {
//              (0x80000004L).U
//            } else {
//              (0x80000000L + i * 8).U
//            }
//          }
//
//          // --------------------
//          // test
//          // --------------------
//          dut.io.pc.expectDequeueSeq(split_seq1)
//
//          split_seq2.foreach { pc =>
//            dut.io.pc.expectDequeue(pc)
//            dut.clock.step(Gen.choose(1, 10).sample.get)
//          }
//
//          fork {
//            dut.io.commit_redirect_pc.enqueue(redirect_pc)
//            dut.clock.step(1)
//          }.fork {
//            dut.io.pc.expectDequeue((expect_pc_seq.last.litValue + 8).U)
//            dut.io.pc.expectDequeueSeq(after_redirect_pc)
//          }.join()
//      }
//  }
//
//  private def init_port(dut: PCGenStage): Unit = {
//    dut.io.redirect_pc.initSource().setSourceClock(dut.clock)
//    dut.io.pc.initSink().setSinkClock(dut.clock)
//    dut.clock.step(5)
//  }
//}
