package leesum
import chisel3._
import chiseltest._
import leesum.axi4.StreamFork
import org.scalatest.freespec.AnyFreeSpec

class StreamForkTest extends AnyFreeSpec with ChiselScalatestTester {

  "StreamFork_test1" in {
    test(new StreamFork(UInt(64.W), 2, synchronous = false))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.input.initSource().setSourceClock(dut.clock)
        dut.io.outputs.foreach(_.initSink().setSinkClock(dut.clock))

        dut.clock.step(5)
        fork {
          dut.clock.step(2)
          dut.io.input.enqueueSeq(Seq(1.U, 2.U, 3.U, 4.U))
          dut.clock.step(4)
          dut.io.input.enqueue(5.U)
          dut.clock.step(1)
          dut.io.input.enqueue(6.U)
          dut.clock.step(1)
        }.fork {
          dut.clock.step(3)
          dut.io.outputs(0).expectDequeueSeq(Seq(1.U, 2.U))
          dut.clock.step(1)
          dut.io.outputs(0).expectDequeueSeq(Seq(3.U, 4.U))
          dut.clock.step(4)
          dut.io.outputs(0).expectDequeue(5.U)
          dut.clock.step(1)
          dut.io.outputs(0).expectDequeue(6.U)
          dut.clock.step(1)
        }.fork {
          dut.clock.step(1)
          dut.io.outputs(1).expectDequeueSeq(Seq(1.U, 2.U))
          dut.clock.step(1)
          dut.io.outputs(1).expectDequeueSeq(Seq(3.U, 4.U, 5.U, 6.U))
          dut.clock.step(1)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)
      }
  }
  "StreamFork_test2" in {
    test(new StreamFork(UInt(64.W), 2, synchronous = true))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.input.initSource().setSourceClock(dut.clock)
        dut.io.outputs.foreach(_.initSink().setSinkClock(dut.clock))

        dut.clock.step(5)
        fork {
          dut.clock.step(2)
          dut.io.input.enqueueSeq(Seq(1.U, 2.U, 3.U, 4.U))
          dut.clock.step(4)
          dut.io.input.enqueue(5.U)
          dut.clock.step(1)
          dut.io.input.enqueue(6.U)
          dut.clock.step(1)
        }.fork {
          dut.clock.step(3)
          dut.io.outputs(0).expectDequeueSeq(Seq(1.U, 2.U))
          dut.clock.step(1)
          dut.io.outputs(0).expectDequeueSeq(Seq(3.U, 4.U))
          dut.clock.step(4)
          dut.io.outputs(0).expectDequeue(5.U)
          dut.clock.step(1)
          dut.io.outputs(0).expectDequeue(6.U)
          dut.clock.step(1)
        }.fork {
          dut.clock.step(1)
          dut.io.outputs(1).expectDequeueSeq(Seq(1.U, 2.U))
          dut.clock.step(1)
          dut.io.outputs(1).expectDequeueSeq(Seq(3.U, 4.U, 5.U, 6.U))
          dut.clock.step(1)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)
      }
  }
}
