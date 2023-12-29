package leesum
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import leesum.Utils.PLRU
import leesum.mmu_sv39.{SV39PageSize, TLBEntry, TLB_L1}
import org.scalatest.freespec.AnyFreeSpec
class PLRUTest extends AnyFreeSpec with ChiselScalatestTester {
  "PLRU_lookupTest" in {
    test(new PLRU(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ----------------
        // Init ports
        // ----------------
        init_ports(dut)

        val visit_seq = Seq(0, 1, 2, 3, 3, 2, 1, 0)

        for (i <- visit_seq.indices) {

          timescope {
            dut.io.update_valid.poke(true.B)
            dut.io.update_data.poke(visit_seq(i).U)
            dut.clock.step(1)
            val out = dut.io.out.peek().litValue
            println(s"out: ${out}")
          }
        }
      }
  }

  private def init_ports(dut: PLRU): Unit = {
    dut.io.update_valid.poke(false.B)
    dut.io.update_data.poke(0.U)
  }
}
