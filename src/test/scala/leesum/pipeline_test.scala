package leesum

import chisel3._
import chiseltest.{ChiselScalatestTester, _}
import leesum.test_utils.gen_rand_uint
import org.scalatest.freespec.AnyFreeSpec

class pipeline_test extends AnyFreeSpec with ChiselScalatestTester {

  "pipeline push and push" in {
    test(new PipeLine(UInt(64.W)))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) {
        dut =>
          dut.io.in.initSource().setSourceClock(dut.clock)
          dut.io.out.initSink().setSinkClock(dut.clock)
          dut.io.flush.poke(false.B)
          dut.clock.step(5)

          val Uint_Gen = gen_rand_uint(64)

          val test_in_seq = 0.until(100).map(_ => Uint_Gen.sample.get)
          val test_out_seq = test_in_seq

          fork {
            dut.io.in.enqueueSeq(test_in_seq)
          }.fork {
            dut.clock.step(5)
            dut.io.out.expectDequeueSeq(test_out_seq)
          }.join()
      }
  }
}
