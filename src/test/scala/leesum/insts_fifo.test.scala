package leesum

import Chisel.Cat
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals.{
  AddObjectLiteralConstructor,
  AddVecLiteralConstructor
}
import chiseltest._
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import chiseltest.simulator.WriteVcdAnnotation

class InistFifo_test extends AnyFreeSpec with ChiselScalatestTester {

  def gen_rand(): Gen[InstsItem] = {
    // Generate a random hex string with 8 characters and prefix it with "x"
    // such as "x12345678", "xabcdef12", etc.
    // and than convert it to UInt(32.W)
    val inst_string_gen =
      Gen.listOfN(8, Gen.hexChar).map("x" + _.mkString).map(_.U(32.W))

    // Generate a random hex string with 8 characters and prefix it with "x8"
    // such as "x81234567", "x8abcdef1", etc.
    // and than convert it to UInt(32.W)
    val pc_string_gen =
      Gen
        .listOfN(7, Gen.hexChar)
        .map("x8" + _.mkString)
        .map(_.U(32.W))

    val bool_gen = Gen.oneOf(true.B, false.B)

    val inst_entry_gen = for {
      inst <- inst_string_gen
      inst_valid <- bool_gen
      inst_rvc <- bool_gen
      inst_pc <- pc_string_gen
    } yield {
      val item = (new INSTEntry).Lit(
        _.inst -> inst,
        _.valid -> inst_valid,
        _.rvc -> inst_rvc,
        _.pc -> inst_pc
      )
      item
    }

    val insts_item_gen = for {
      insts <- Gen.listOfN(4, inst_entry_gen)
    } yield {
      val item = (new InstsItem).Lit(
        _.insts_vec -> Vec.Lit(insts: _*)
      )
      item
    }
    insts_item_gen
  }
  "flush_test" in {
    test(new InstsFifo)
      .withAnnotations(Seq(IcarusBackendAnnotation, WriteVcdAnnotation)) {
        dut =>
          {
            dut.io.in.initSource()
            dut.io.in.setSourceClock(dut.clock)
            dut.io.out.initSink()
            dut.io.out.setSinkClock(dut.clock)


            val insts_item_gen =
              gen_rand()

            // prepare test data

            // use Gen.listOfN to generate a list of 100 items as input data
            val input_data_seq = Gen
              .listOfN(100, insts_item_gen)
              .sample
              .get
            // reference data,only valid insts will be pushed into the fifo
            val ref_data_seq =
              input_data_seq.flatten(_.insts_vec).filter(_.valid.litToBoolean)

            dut.clock.step(5)
            dut.io.in.enqueue(input_data_seq(0))
            dut.io.in.enqueue(input_data_seq(1))
            dut.io.flush.poke(true.B)
            dut.clock.step(1)
            dut.io.flush.poke(false.B)

            fork {
              // push inputs into the calculator, stall for 11 cycles one third of the way
              val (seq1, seq2) =
                input_data_seq.splitAt(input_data_seq.length / 3)
              dut.io.in.enqueueSeq(seq1)
              dut.clock.step(11)
              dut.io.in.enqueueSeq(seq2)
            }.fork {
              // retrieve computations from the calculator, stall for 10 cycles one half of the way
              val (seq1, seq2) = ref_data_seq.splitAt(ref_data_seq.length / 4)
              dut.io.out.expectDequeueSeq(seq1)
              dut.clock.step(200)
              dut.io.out.expectDequeueSeq(seq2)

            }.join()
          }
      }
  }

  "fifo_push_test" in {
    test(new InstsFifo).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      {
        dut.io.in.initSource()
        dut.io.in.setSourceClock(dut.clock)
        dut.io.out.initSink()
        dut.io.out.setSinkClock(dut.clock)

        val insts_item_gen =
          gen_rand()

        // prepare test data

        // use Gen.listOfN to generate a list of 100 items as input data
        val input_data_seq = Gen
          .listOfN(100, insts_item_gen)
          .sample
          .get

        // reference data,only valid insts will be pushed into the fifo
        val ref_data_seq =
          input_data_seq.flatten(_.insts_vec).filter(_.valid.litToBoolean)

        dut.clock.step(5)
        fork {
          // push inputs into the calculator, stall for 11 cycles one third of the way
          val (seq1, seq2) = input_data_seq.splitAt(input_data_seq.length / 3)
          dut.io.in.enqueueSeq(seq1)
          dut.clock.step(11)
          dut.io.in.enqueueSeq(seq2)
        }.fork {
          // retrieve computations from the calculator, stall for 10 cycles one half of the way
          val (seq1, seq2) = ref_data_seq.splitAt(ref_data_seq.length / 4)
          dut.io.out.expectDequeueSeq(seq1)
          dut.clock.step(200)
          dut.io.out.expectDequeueSeq(seq2)
        }.join()

      }
    }

  }

}
