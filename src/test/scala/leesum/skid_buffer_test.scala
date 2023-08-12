package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import leesum.axi4.AXIDef.{
  BURST_INCR,
  SIZE_1,
  SIZE_128,
  SIZE_16,
  SIZE_2,
  SIZE_32,
  SIZE_4,
  SIZE_64,
  SIZE_8
}
import leesum.axi4.{
  AXI4Memory,
  AXIAddr,
  AXIAddressChannel,
  axi_addr,
  skid_buffer,
  skid_buffer_cut_valid
}
import leesum.test_utils.{int2UInt64, long2UInt64, long2Ulong}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class skid_buffer_test extends AnyFreeSpec with ChiselScalatestTester {

  "skid_buffer_test" in {
    test(new skid_buffer(UInt(32.W), CUT_VALID = true, CUT_READY = true))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.in.initSource()
        dut.io.in.setSourceClock(dut.clock)
        dut.io.out.initSink()
        dut.io.out.setSinkClock(dut.clock)

        dut.clock.step(5)
        val data_seq = Seq(1, 2, 3, 4, 5, 6, 7, 8).map(_.U)

        fork {
          val (date_seq1, data_seq2) = data_seq.splitAt(4)
          dut.io.in.enqueueSeq(date_seq1)
          dut.clock.step(10)
          dut.io.in.enqueueSeq(data_seq2)
        }.fork {
          val (date_seq1, data_seq2) = data_seq.splitAt(2)
          dut.clock.step(3)
          dut.io.out.expectDequeueSeq(date_seq1)
          dut.clock.step(5)
          dut.io.out.expectDequeueSeq(data_seq2)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)
      }
  }

  "skid_buffer_test_on_stop" in {
    test(new skid_buffer(UInt(32.W), CUT_VALID = true, CUT_READY = true))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.in.initSource()
        dut.io.in.setSourceClock(dut.clock)
        dut.io.out.initSink()
        dut.io.out.setSinkClock(dut.clock)

        dut.clock.step(5)
        val data_seq = Seq(1, 2, 3, 4, 5, 6, 7, 8).map(_.U)

        fork {
          dut.io.in.enqueueSeq(data_seq)
        }.fork {
          dut.clock.step(10)
          dut.io.out.expectDequeueSeq(data_seq)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        fork {
          dut.io.in.enqueueSeq(data_seq)
        }.fork {
          dut.io.out.expectDequeueSeq(data_seq)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        dut.io.in.enqueueSeq(data_seq.take(2))
        fork {
          dut.clock.step(1)
          dut.io.in.enqueueSeq(data_seq.drop(2))
        }.fork {
          dut.io.out.expectDequeueSeq(data_seq)
        }.joinAndStep(dut.clock)
      }
  }
}
