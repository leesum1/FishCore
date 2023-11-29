package leesum
import chiseltest._
import chisel3._
import leesum.TestUtils.gen_rand_uint
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class ValidFIFOTest extends AnyFreeSpec with ChiselScalatestTester {

  "multi_ports_valid_fifo_flush_test" in {
    test(new DummyMultiPortFIFO(UInt(64.W), 4, 2, 2))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ----------------
        // Init ports
        // ----------------
        dut.io.in.foreach(_.initSource().setSourceClock(dut.clock))
        dut.io.out.foreach(_.initSink().setSinkClock(dut.clock))
        dut.io.flush.poke(false.B)
        dut.clock.step(5)
        // -------------------
        // prepare test data
        // -------------------
        val before_flush_input_seq =
          Gen.listOfN(6, gen_rand_uint(64)).sample.get

        val after_flush_input_seq =
          Gen.listOfN(2000, gen_rand_uint(64)).sample.get
        val expected_seq = after_flush_input_seq

        // -------------------
        // flush test
        // -------------------

        // flush
        dut.io.in(0).enqueueSeq(before_flush_input_seq.take(2))
        fork {
          dut.io.in(0).enqueue(before_flush_input_seq(2))
        }.fork {
          dut.io.in(1).enqueue(before_flush_input_seq(3))
        }.fork {
          timescope {
            dut.io.flush.poke(true.B)
            dut.clock.step(1)
          }
        }.joinAndStep(dut.clock)

        // after flush test
        // --------------------------------------------
        // two port push, two port pop (with bubble)
        // --------------------------------------------
        fork {
          multi_port_push(dut, after_flush_input_seq, true)
        }.fork {
          multi_port_pop(dut, expected_seq, true)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        fork {
          multi_port_push(dut, after_flush_input_seq, false)
        }.fork {
          multi_port_pop(dut, expected_seq, true)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        fork {
          multi_port_push(dut, after_flush_input_seq, true)
        }.fork {
          multi_port_pop(dut, expected_seq, false)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

      }
  }

  "multi_ports_valid_fifo_push_pop_test" in {
    test(new DummyMultiPortFIFO(UInt(64.W), 4, 2, 2))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ----------------
        // Init ports
        // ----------------
        dut.io.in.foreach(_.initSource().setSourceClock(dut.clock))
        dut.io.out.foreach(_.initSink().setSinkClock(dut.clock))
        dut.io.flush.poke(false.B)
        dut.clock.step(5)
        // ----------------
        // prepare test data
        // ----------------
        val input_seq = Gen.listOfN(5000, gen_rand_uint(64)).sample.get
        val expected_seq = input_seq

        // ---------------------------------------------
        // one port push, one port pop (without bubble)
        // ---------------------------------------------
        fork {
          dut.io.in(0).enqueueSeq(input_seq)
        }.fork {
          dut.io.out(0).expectDequeueSeq(expected_seq)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)
        // ------------------------------------------
        // one port push, one port pop (with bubble)
        // ------------------------------------------
        fork {
          input_seq.foreach(data => {
            dut.clock.step(Gen.chooseNum(1, 10).sample.get)
            dut.io.in(0).enqueue(data)
          })
        }.fork {
          expected_seq.foreach(data => {
            dut.clock.step(Gen.chooseNum(1, 10).sample.get)
            dut.io.out(0).expectDequeue(data)
          })
        }.joinAndStep(dut.clock)
        dut.clock.step(5)
        // --------------------------------------------
        // two port push, one port pop (without bubble)
        // --------------------------------------------
        fork {
          multi_port_push(dut, input_seq, false)
        }.fork {
          dut.io.out(0).expectDequeueSeq(expected_seq)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)
        // --------------------------------------------
        // two port push,  one port pop (with bubble)
        // --------------------------------------------
        fork {
          multi_port_push(dut, input_seq, true)
        }.fork {
          expected_seq.foreach(data => {
            dut.clock.step(Gen.chooseNum(1, 10).sample.get)
            dut.io.out(0).expectDequeue(data)
          })
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        // --------------------------------------------
        // two port push, two port pop (without bubble)
        // --------------------------------------------
        fork {
          multi_port_push(dut, input_seq, false)
        }.fork {
          multi_port_pop(dut, expected_seq, false)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        // --------------------------------------------
        // two port push, two port pop (with bubble)
        // --------------------------------------------
        fork {
          multi_port_push(dut, input_seq, true)
        }.fork {
          multi_port_pop(dut, expected_seq, true)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        fork {
          multi_port_push(dut, input_seq, false)
        }.fork {
          multi_port_pop(dut, expected_seq, true)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        fork {
          multi_port_push(dut, input_seq, true)
        }.fork {
          multi_port_pop(dut, expected_seq, false)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

      }
  }

  private def multi_port_pop(
      dut: DummyMultiPortFIFO[UInt],
      expected_seq: List[UInt],
      with_bubble: Boolean
  ): Unit = {
    var pop_ptr = 0
    while (pop_ptr < expected_seq.length) {
      val valid0 = dut.io.out(0).valid.peek().litToBoolean
      val valid1 = dut.io.out(1).valid.peek().litToBoolean
      if (valid0 && valid1) {
        fork {
          dut.io.out(0).expectDequeue(expected_seq(pop_ptr))
        }.fork {
          if (pop_ptr + 1 < expected_seq.length) {
            dut.io.out(1).expectDequeue(expected_seq(pop_ptr + 1))
          } else {
            dut.clock.step(1)
          }
        }.joinAndStep(dut.clock)

        pop_ptr += 2
      } else if (valid0) {
        dut.io.out(0).expectDequeue(expected_seq(pop_ptr))
        pop_ptr += 1
      } else if (valid1) {
        assert(false, "will not happen")
      }

      if (with_bubble) {
        dut.clock.step(Gen.chooseNum(1, 10).sample.get)
      } else {
        dut.clock.step(1)
      }
    }
  }

  private def multi_port_push(
      dut: DummyMultiPortFIFO[UInt],
      input_seq: List[UInt],
      with_bubble: Boolean
  ): Unit = {
    var push_ptr = 0
    while (push_ptr < input_seq.length) {
      val ready0 = dut.io.in(0).ready.peek().litToBoolean
      val ready1 = dut.io.in(1).ready.peek().litToBoolean
      if (ready0 && ready1) {
        fork {
          dut.io.in(0).enqueue(input_seq(push_ptr))
        }.fork {
          if (push_ptr + 1 < input_seq.length) {
            dut.io.in(1).enqueue(input_seq(push_ptr + 1))
          }
          dut.clock.step(1)
        }.joinAndStep(dut.clock)
        push_ptr += 2
      } else if (ready0) {
        dut.io.in(0).enqueue(input_seq(push_ptr))
        push_ptr += 1
      } else if (ready1) {
        assert(false, "will not happen")
      }
      if (with_bubble) {
        dut.clock.step(Gen.chooseNum(1, 10).sample.get)
      } else {
        dut.clock.step(1)
      }
    }
  }
}
