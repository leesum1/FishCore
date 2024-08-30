package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.Decoupled
import chiseltest._
import leesum.Cache.{LoadDcacheReq, LoadDcacheResp}
import leesum.lsu.{LoadQueue, LoadQueueIn, LoadWriteBack, StoreBypassData}
import leesum.TestUtils.long2UInt64
import leesum.dbg.{DebugModuleConfig, JtagDTM, JtagState}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class JtagDTMTest extends AnyFreeSpec with ChiselScalatestTester {
  val dm_config = new DebugModuleConfig()
  "JtagStateIdleTest" in {
    test(
      new JtagDTM(dm_config)
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.jtag.tck.poke(false.B)
        dut.io.jtag.tms.poke(false.B)
        dut.io.jtag.tdi.poke(false.B)

        for (_ <- 0 until 20000) {
          // randomize tms
          for (i <- 0 until 50) {
            val random_bool = Gen.oneOf(true, false).sample.get
            dut.io.jtag.tms.poke(random_bool.B)
            dut.clock.step(1)
          }
          // 5 cycles of tms = 1
          for (i <- 0 until 5) {
            dut.io.jtag.tms.poke(true.B)
            dut.clock.step(1)
          }
          dut.io.jtag_state.expect(JtagState.TestLogicReset)
        }
      }
  }
}
