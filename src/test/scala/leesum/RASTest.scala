package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import leesum.Utils.PLRU
import leesum.bpu.RAS
import leesum.mmu_sv39.{SV39PageSize, TLBEntry, TLB_L1}
import org.scalatest.freespec.AnyFreeSpec
class RASTest extends AnyFreeSpec with ChiselScalatestTester {

  val push_seq1 = Seq(100, 200, 300, 400)
  val push_seq2 = Seq(500, 600, 700, 800)

  "RAS_PUSH_POP_Test" in {
    test(new RAS(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ----------------
        // Init ports
        // ----------------
        init_port(dut)

        // ----------------
        // Push
        // ----------------
        push_seq1.foreach { target =>
          timescope {
            dut.io.push_cond.poke(true.B)
            dut.io.push_target.poke(target.U)
            dut.clock.step()
          }
        }
        dut.io.ras_full.expect(true.B)

        // ----------------
        // Pop
        // ----------------
        push_seq1.reverse.foreach { target =>
          timescope {
            dut.io.ras_top.expect(target.U)
            dut.io.pop_cond.poke(true.B)
            dut.clock.step()
          }
        }
        dut.io.ras_empty.expect(true.B)
        dut.io.ras_full.expect(false.B)

        dut.clock.step(10)

      }
  }

  "RAS_PUSH_POP_Overflow_Test" in {
    test(new RAS(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ----------------
        // Init ports
        // ----------------
        init_port(dut)

        // ----------------
        // Push
        // ----------------
        push_seq1.foreach { target =>
          timescope {
            dut.io.push_cond.poke(true.B)
            dut.io.push_target.poke(target.U)
            dut.clock.step()
          }
        }
        dut.io.ras_full.expect(true.B)

        // ----------------
        // Push overflow
        // ----------------
        push_seq2.foreach { target =>
          timescope {
            dut.io.push_cond.poke(true.B)
            dut.io.push_target.poke(target.U)
            dut.clock.step()
          }
        }
        dut.io.ras_full.expect(true.B)
        // ----------------
        // Pop
        // ----------------
        push_seq2.reverse.foreach { target =>
          timescope {
            dut.io.ras_top.expect(target.U)
            dut.io.pop_cond.poke(true.B)
            dut.clock.step()
          }
        }
        dut.io.ras_empty.expect(true.B)
        dut.io.ras_full.expect(false.B)

        dut.clock.step(10)

      }
  }

  "RAS_Refill_Test" in {
    test(new RAS(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ----------------
        // Init ports
        // ----------------
        init_port(dut)

        // ----------------
        // Push
        // ----------------
        push_seq1.foreach { target =>
          timescope {
            dut.io.push_cond.poke(true.B)
            dut.io.push_target.poke(target.U)
            dut.clock.step()
          }
        }
        dut.io.ras_full.expect(true.B)

        // ----------------
        // Refill
        // ----------------
        timescope {
          dut.io.refill_en.poke(true.B)
          for (i <- 0 until 4) {
            dut.io.refill_ras_stack(i).poke(push_seq2(i))
          }
          dut.io.refill_ras_occupied.poke(4.U)
          dut.io.refill_top_ptr.poke(3.U)
          dut.clock.step()
        }

        dut.io.ras_full.expect(true.B)

        // ----------------
        // Pop
        // ----------------
        push_seq2.reverse.foreach { target =>
          timescope {
            dut.io.ras_top.expect(target.U)
            dut.io.pop_cond.poke(true.B)
            dut.clock.step()
          }
        }
        dut.io.ras_empty.expect(true.B)
        dut.io.ras_full.expect(false.B)

        dut.clock.step(10)

        // ----------------
        // Refill
        // ----------------
        timescope {
          dut.io.refill_en.poke(true.B)
          for (i <- 0 until 4) {
            dut.io.refill_ras_stack(i).poke(push_seq1(i))
          }
          dut.io.refill_ras_occupied.poke(2.U)
          dut.io.refill_top_ptr.poke(1.U)
          dut.clock.step()
        }

        dut.io.ras_full.expect(false.B)

        // ----------------
        // Pop
        // ----------------
        push_seq1.take(2).reverse.foreach { target =>
          timescope {
            dut.io.ras_top.expect(target.U)
            dut.io.pop_cond.poke(true.B)
            dut.clock.step()
          }
        }
        dut.io.ras_empty.expect(true.B)
        dut.io.ras_full.expect(false.B)

      }
  }

  private def init_port(dut: RAS): Unit = {
    dut.io.push_cond.poke(false.B)
    dut.io.pop_cond.poke(false.B)
    dut.io.refill_en.poke(false.B)
  }
}
