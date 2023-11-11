//package leesum
//
//import chisel3._
//import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
//import chisel3.experimental.VecLiterals.AddObjectLiteralConstructor
//import chiseltest._
//import chiseltest.simulator.WriteFstAnnotation
//import org.scalacheck.Gen
//import org.scalatest.freespec.AnyFreeSpec
//
//class InstFifo_test extends AnyFreeSpec with ChiselScalatestTester {
//
//  private def multi_port_pop(
//      dut: InstsFIFO,
//      expected_seq: List[INSTEntry],
//      with_bubble: Boolean
//  ): Unit = {
//    var pop_ptr = 0
//    while (pop_ptr < expected_seq.length) {
//      val valid0 = dut.io.pop(0).valid.peek().litToBoolean
//      val valid1 = dut.io.pop(1).valid.peek().litToBoolean
//      if (valid0 && valid1) {
//        fork {
//          dut.io.pop(0).expectDequeue(expected_seq(pop_ptr))
//        }.fork {
//          if (pop_ptr + 1 < expected_seq.length) {
//            dut.io.pop(1).expectDequeue(expected_seq(pop_ptr + 1))
//          } else {
//            dut.clock.step(1)
//          }
//        }.joinAndStep(dut.clock)
//
//        pop_ptr += 2
//      } else if (valid0) {
//        dut.io.pop(0).expectDequeue(expected_seq(pop_ptr))
//        pop_ptr += 1
//      } else if (valid1) {
//        assert(false, "will not happen")
//      }
//
//      if (with_bubble) {
//        dut.clock.step(Gen.chooseNum(1, 10).sample.get)
//      } else {
//        dut.clock.step(1)
//      }
//    }
//  }
//
//  def gen_rand(): Gen[InstsItem] = {
//    // Generate a random hex string with 8 characters and prefix it with "x"
//    // such as "x12345678", "xabcdef12", etc.
//    // and than convert it to UInt(32.W)
//    val inst_string_gen =
//      Gen.listOfN(8, Gen.hexChar).map("x" + _.mkString).map(_.U(32.W))
//
//    // Generate a random hex string with 8 characters and prefix it with "x8"
//    // such as "x81234567", "x8abcdef1", etc.
//    // and than convert it to UInt(32.W)
//    val pc_string_gen =
//      Gen
//        .listOfN(7, Gen.hexChar)
//        .map("x8" + _.mkString)
//        .map(_.U(32.W))
//
//    val bool_gen = Gen.oneOf(true.B, false.B)
//
//    val inst_entry_gen = for {
//      inst <- inst_string_gen
//      inst_valid <- bool_gen
//      inst_rvc <- bool_gen
//      inst_pc <- pc_string_gen
//    } yield {
//      val item = (new INSTEntry).Lit(
//        _.inst -> inst,
//        _.valid -> inst_valid,
//        _.rvc -> inst_rvc,
//        _.pc -> inst_pc
//      )
//      item
//    }
//
//    val insts_item_gen = for {
//      insts <- Gen.listOfN(4, inst_entry_gen)
//    } yield {
//      val item = (new InstsItem).Lit(
//        _.insts_vec -> Vec.Lit(insts: _*)
//      )
//      item
//    }
//    insts_item_gen
//  }
//
//  "InstFIFO2_flush_test" in {
//    test(new InstsFIFO).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//    ) { dut =>
//      dut.io.push.initSource()
//      dut.io.push.setSourceClock(dut.clock)
//      dut.io.pop.foreach(_.initSink().setSinkClock(dut.clock))
//
//      val insts_item_gen =
//        gen_rand()
//
//      // prepare test data
//
//      // use Gen.listOfN to generate a list of 100 items as input data
//      val input_data_seq = Gen
//        .listOfN(2000, insts_item_gen)
//        .sample
//        .get
//
//      // reference data,only valid insts will be pushed into the fifo
//      val ref_data_seq =
//        input_data_seq.flatten(_.insts_vec).filter(_.valid.litToBoolean)
//
//      dut.clock.step(5)
//      // --------------------
//      // flush test
//      // --------------------
//      dut.io.push.enqueue(insts_item_gen.sample.get)
//      fork {
//        dut.io.push.enqueue(insts_item_gen.sample.get)
//      }.fork {
//        dut.io.flush.poke(true.B)
//        dut.clock.step(1)
//        dut.io.flush.poke(false.B)
//        dut.clock.step(1)
//      }.join()
//      // ---------------------------
//      // push pop test after flush
//      // ---------------------------
//      fork {
//        val (seq1, seq2) = input_data_seq.splitAt(input_data_seq.length / 3)
//        dut.io.push.enqueueSeq(seq1)
//        dut.clock.step(11)
//        dut.io.push.enqueueSeq(seq2)
//      }.fork {
//        multi_port_pop(dut, ref_data_seq, true)
//      }.join()
//      dut.clock.step(5)
//    }
//  }
//
//  "InstsFIFO2_push_pop_test" in {
//    test(new InstsFIFO).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//    ) { dut =>
//      // --------------------
//      // init port
//      // --------------------
//      dut.io.push.initSource()
//      dut.io.push.setSourceClock(dut.clock)
//      dut.io.pop.foreach(_.initSink().setSinkClock(dut.clock))
//
//      // --------------------
//      // prepare test data
//      // --------------------
//      val insts_item_gen =
//        gen_rand()
//
//      // prepare test data
//
//      // use Gen.listOfN to generate a list of 100 items as input data
//      val input_data_seq = Gen
//        .listOfN(2000, insts_item_gen)
//        .sample
//        .get
//
//      // reference data,only valid insts will be pushed into the fifo
//      val ref_data_seq =
//        input_data_seq.flatten(_.insts_vec).filter(_.valid.litToBoolean)
//
//      // ----------------------------
//      // push pop test without bubble
//      // ----------------------------
//      dut.clock.step(5)
//      fork {
//        dut.io.push.enqueueSeq(input_data_seq)
//      }.fork {
//        multi_port_pop(dut, ref_data_seq, false)
//      }.join()
//      dut.clock.step(5)
//
//      // --------------------
//      // push pop test with bubble
//      // --------------------
//      dut.clock.step(5)
//      fork {
//        input_data_seq.foreach(in => {
//          dut.io.push.enqueue(in)
//          dut.clock.step(Gen.chooseNum(1, 10).sample.get)
//        })
//
//      }.fork {
//        multi_port_pop(dut, ref_data_seq, true)
//      }.join()
//      dut.clock.step(5)
//    }
//  }
//
//  "CompressInstsItem_test" in {
//    test(new CompressInstsItem()).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//    ) { dut =>
//      val insts_item_gen =
//        gen_rand()
//
//      // -------------------
//      // prepare test data
//      // -------------------
//
//      // use Gen.listOfN to generate a list of 100 items as input data
//      val input_data_seq = Gen
//        .listOfN(2000, insts_item_gen)
//        .sample
//        .get
//
//      // reference data,only valid insts will be pushed into the fifo
//      var ref_data_seq =
//        input_data_seq.flatten(_.insts_vec).filter(_.valid.litToBoolean)
//
//      dut.clock.step(5)
//
//      // -------------------
//      // start test
//      // -------------------
//      for (in <- input_data_seq) {
//        // enqueue input data
//        dut.io.in.poke(in)
//        dut.clock.step(1)
//        // check output data
//        val out_valid = dut.io.out.peek()
//        for (i <- 0 until out_valid.insts_vec.length)
//          if (out_valid.insts_vec(i).valid.litToBoolean) {
//            dut.io.out.insts_vec(i).expect(ref_data_seq.head)
//            // remove the head element
//            ref_data_seq = ref_data_seq.tail
//          }
//      }
//
//      dut.clock.step(5)
//    }
//  }
//}
