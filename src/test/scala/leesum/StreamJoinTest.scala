package leesum

import chisel3._
import chiseltest._
import leesum.axi4.{StreamFork, StreamJoin}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class StreamJoinTest extends AnyFreeSpec with ChiselScalatestTester {

  "StreamJoin_test1" in {
    test(new StreamJoin(2))
      .withAnnotations(
        Seq(VcsBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // -------------------
        // init ports
        // -------------------
        dut.io.in.foreach(_.initSource().setSourceClock(dut.clock))
        dut.io.out.initSink().setSinkClock(dut.clock)

        val input_seq1 = Gen.listOfN(100, Gen.oneOf(true, false)).sample.get
        val input_seq2 = Gen.listOfN(100, Gen.oneOf(true, false)).sample.get
        val out_put_seq =
          input_seq1.zip(input_seq2).map { case (a, b) => a ^ b }

        val enq_seq1 = (with_bubble: Boolean) => {
          input_seq1.foreach(in => {
            dut.io.in(0).enqueue(in.B)
            if (with_bubble) dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }
        val enq_seq2 = (with_bubble: Boolean) => {
          input_seq2.foreach(in => {
            dut.io.in(1).enqueue(in.B)
            if (with_bubble) dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }
        val expect_seq = (with_bubble: Boolean) => {
          out_put_seq.foreach(out => {
            dut.io.out.expectDequeue(out.B)
            if (with_bubble) dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }

        val test_template = (with_bubble: Seq[Boolean]) => {
          require(with_bubble.length == 3)
          fork {
            enq_seq1(with_bubble(0))
          }.fork {
            enq_seq2(with_bubble(1))
          }.fork {
            expect_seq(with_bubble(2))
          }.joinAndStep(dut.clock)
          dut.clock.step(5)
        }

        // -------------------
        // test
        // -------------------
        val combinations = for {
          a <- Seq(true, false)
          b <- Seq(true, false)
          c <- Seq(true, false)
        } yield Seq(a, b, c)

        combinations.foreach(test_template)

      }
  }

  "StreamJoin_test2" in {
    test(new StreamJoin(3))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // -------------------
        // init ports
        // -------------------
        dut.io.in.foreach(_.initSource().setSourceClock(dut.clock))
        dut.io.out.initSink().setSinkClock(dut.clock)

        val input_seq1 = Gen.listOfN(100, Gen.oneOf(true, false)).sample.get
        val input_seq2 = Gen.listOfN(100, Gen.oneOf(true, false)).sample.get
        val input_seq3 = Gen.listOfN(100, Gen.oneOf(true, false)).sample.get
        val out_put_seq =
          input_seq1.zip(input_seq2).zip(input_seq3).map { case ((a, b), c) =>
            a ^ b ^ c
          }

        val enq_seq1 = (with_bubble: Boolean) => {
          input_seq1.foreach(in => {
            dut.io.in(0).enqueue(in.B)
            if (with_bubble) dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }
        val enq_seq2 = (with_bubble: Boolean) => {
          input_seq2.foreach(in => {
            dut.io.in(1).enqueue(in.B)
            if (with_bubble) dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }
        val enq_seq3 = (with_bubble: Boolean) => {
          input_seq3.foreach(in => {
            dut.io.in(2).enqueue(in.B)
            if (with_bubble) dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }
        val expect_seq = (with_bubble: Boolean) => {
          out_put_seq.foreach(out => {
            dut.io.out.expectDequeue(out.B)
            if (with_bubble) dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }

        val test_template = (with_bubble: Seq[Boolean]) => {
          require(with_bubble.length == 4)
          fork {
            enq_seq1(with_bubble(0))
          }.fork {
            enq_seq2(with_bubble(1))
          }.fork {
            enq_seq3(with_bubble(2))
          }.fork {
            expect_seq(with_bubble(3))
          }.joinAndStep(dut.clock)
          dut.clock.step(5)
        }

        // -------------------
        // test
        // -------------------
        val combinations = for {
          a <- Seq(true, false)
          b <- Seq(true, false)
          c <- Seq(true, false)
          d <- Seq(true, false)
        } yield Seq(a, b, c, d)

        combinations.foreach(test_template)

      }
  }
}
