package leesum
import chisel3._
import chiseltest._
import leesum.axi4.StreamFork
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class StreamForkTest extends AnyFreeSpec with ChiselScalatestTester {

  "StreamFork_test1" in {
    test(new StreamFork(UInt(64.W), 2, synchronous = false))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.input.initSource().setSourceClock(dut.clock)
        dut.io.outputs.foreach(_.initSink().setSinkClock(dut.clock))

        // -------------------
        // prepare data
        // -------------------
        val input_seq = Gen.listOfN(100, TestUtils.gen_rand_uint(64)).sample.get
        val output_seq = input_seq

        val test_template = (with_bubble: Seq[Boolean]) => {
          require(with_bubble.length == 3)
          fork {
            input_seq.foreach(in => {
              dut.io.input.enqueue(in)
              if (with_bubble(0))
                dut.clock.step(Gen.chooseNum(1, 10).sample.get)
            })
          }.fork {
            output_seq.foreach(out => {
              dut.io.outputs(0).expectDequeue(out)
              if (with_bubble(1))
                dut.clock.step(Gen.chooseNum(1, 10).sample.get)
            })
          }.fork {
            output_seq.foreach(out => {
              dut.io.outputs(1).expectDequeue(out)
              if (with_bubble(2))
                dut.clock.step(Gen.chooseNum(1, 10).sample.get)
            })
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

  "StreamFork_test3" in {
    test(new StreamFork(UInt(64.W), 2, synchronous = false))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.input.initSource().setSourceClock(dut.clock)
        dut.io.outputs.foreach(_.initSink().setSinkClock(dut.clock))

        // -------------------
        // prepare data
        // -------------------
        val input_seq = Gen.listOfN(100, TestUtils.gen_rand_uint(64)).sample.get
        val output_seq = input_seq

        // -------------------
        // test
        // -------------------
        dut.io.outputs(1).ready.poke(true.B)
        dut.clock.step(2)
        fork {
          dut.io.input.enqueue(input_seq(0))
          dut.io.input.enqueue(input_seq(1))
          dut.io.input.enqueue(input_seq(2))
        }.fork {
          dut.io.outputs(0).ready.poke(false.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(true.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(false.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(true.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(false.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(true.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(false.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(true.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(false.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(true.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(false.B)
          dut.clock.step(1)
          dut.io.outputs(0).ready.poke(true.B)
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

        // -------------------
        // prepare data
        // -------------------
        val input_seq = Gen.listOfN(100, TestUtils.gen_rand_uint(64)).sample.get
        val output_seq = input_seq

        val test_template = (with_bubble: Seq[Boolean]) => {
          require(with_bubble.length == 3)
          fork {
            input_seq.foreach(in => {
              dut.io.input.enqueue(in)
              if (with_bubble(0))
                dut.clock.step(Gen.chooseNum(1, 10).sample.get)
            })
          }.fork {
            output_seq.foreach(out => {
              dut.io.outputs(0).expectDequeue(out)
              if (with_bubble(1))
                dut.clock.step(Gen.chooseNum(1, 10).sample.get)
            })
          }.fork {
            output_seq.foreach(out => {
              dut.io.outputs(1).expectDequeue(out)
              if (with_bubble(2))
                dut.clock.step(Gen.chooseNum(1, 10).sample.get)
            })
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

}
