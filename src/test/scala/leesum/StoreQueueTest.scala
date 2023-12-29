package leesum

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.Decoupled
import chiseltest._
import leesum.Cache.StoreDcacheResp
import leesum.lsu.{LoadQueue, LoadQueueIn, LoadWriteBack, StoreBypassData, StoreQueue, StoreQueueIn}
import leesum.TestUtils.{gen_axi_wstrb, gen_rand_uint, long2UInt64}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class StoreQueueDut extends Module {

  val io = IO(new Bundle {
    val store_req = Flipped(Decoupled(new StoreQueueIn))
    val store_commit = Flipped(Decoupled(Bool()))

    val load_req = Flipped(Decoupled(new LoadQueueIn))
    val load_resp = Decoupled(new LoadWriteBack)
    // from commit stage, when commit a mmio instruction, set mmio_commit to true
    val mmio_commit = Flipped(Decoupled(Bool()))

    val flush = Input(Bool())
  })
  val store_queue = Module(new StoreQueue())
  val load_queue = Module(new LoadQueue())
  val dcache = Module(new DummyDCacheDut())

  load_queue.io.in <> io.load_req
  load_queue.io.load_wb <> io.load_resp
  load_queue.io.mmio_commit <> io.mmio_commit
  load_queue.io.flush := io.flush

  store_queue.io.in <> io.store_req
  store_queue.io.store_commit <> io.store_commit
  store_queue.io.flush := io.flush
  store_queue.io.store_bypass.paddr := io.store_req.bits.paddr
  store_queue.io.store_bypass.valid := false.B

  dcache.io.flush := io.flush

  dcache.io.store_req <> store_queue.io.dcache_req
  dcache.io.store_resp <> store_queue.io.dcache_resp
  dcache.io.load_req <> load_queue.io.dcache_req
  dcache.io.load_resp <> load_queue.io.dcache_resp

}

object gen_store_queue_dut_verilog extends App {
  GenVerilogHelper(new StoreQueueDut())
}

class StoreQueueTest extends AnyFreeSpec with ChiselScalatestTester {

  val size1 = 0
  val size2 = 1
  val size4 = 2
  val size8 = 3

  def default_store_bypass() = {
    (new StoreBypassData).Lit(
      _.wstrb -> 0.U,
      _.wdata -> 0.U,
      _.is_mmio -> false.B,
      _.valid -> false.B
    )
  }
  def default_store_resp() = {
    val ex = (new ExceptionEntry).Lit(
      _.valid -> false.B,
      _.tval -> 0.U,
      _.cause -> ExceptionCause.store_access
    )
    (new StoreDcacheResp).Lit(
      _.exception -> ex
    )
  }

  // TODO: not implement
  def gen_store_req(
      paddr: Int,
      store_data: Long,
      size: Int,
      is_mmio: Boolean = false
  ) = {
    require(TestUtils.check_aligned(paddr, size), "paddr must be aligned")
    val wstrb = gen_axi_wstrb(paddr, size)
    (new StoreQueueIn).Lit(
      _.paddr -> paddr.U,
      _.size -> size.U,
      _.wdata -> long2UInt64(store_data),
      _.wstrb -> wstrb.U(8.W),
      _.is_mmio -> is_mmio.B,
      _.trans_id -> 0.U
    )
  }

  def gen_load_req(
      paddr: Int,
      size: Int,
      trans_id: Int,
      is_mmio: Boolean = false
  ) = {
    require(TestUtils.check_aligned(paddr, size), "paddr must be aligned")
    (new LoadQueueIn).Lit(
      _.paddr -> paddr.U,
      _.size -> size.U,
      _.is_mmio -> is_mmio.B,
      _.trans_id -> TestUtils.int2UInt32(trans_id),
      // TODO: not implement
      _.sign_ext -> false.B,
      // TODO: not implement
      _.store_bypass -> default_store_bypass()
    )
  }

  def gen_load_resp(addr: BigInt, data: Long, size: Int, trans_id: Int) = {
    val offset = ((addr % 8) * 8).toLong
    val data_shifted = data >> offset
    val data_masked = size match {
      case 0 => data_shifted & 0xffL
      case 1 => data_shifted & 0xffffL
      case 2 =>
        data_shifted & 0xffffffffL
      case 3 => data_shifted
    }
    // TODO: sign_ext
    (new LoadWriteBack).Lit(
      _.tran_id -> TestUtils.int2UInt32(trans_id),
      _.rdata -> long2UInt64(data_masked)
    )
  }

  def gen_store_req_input_seq(size: Int): Seq[StoreQueueIn] = {
    require(size >= 0 && size <= 3, "size must be in range [0, 3]")
    val real_size = size match {
      case 0 => 1
      case 1 => 2
      case 2 => 4
      case 3 => 8
    }
    0.until(2048, real_size)
      .map(addr => {
        gen_store_req(addr, Gen.long.sample.get, size)
      })
  }

  def pop_store_queue(dut: StoreQueue): Unit = {
    // pop one store_req from store queue
    timescope {
      dut.io.dcache_req.ready.poke(true.B)
      dut.clock.step(1)
    }
    dut.io.dcache_resp.enqueue(default_store_resp())
  }

  private def StoreQueueTestBackToBack(
      dut: StoreQueueDut,
      input_store_req_seq: Seq[StoreQueueIn],
      input_load_req_seq: Seq[LoadQueueIn],
      expect_load_resp_seq: Seq[LoadWriteBack]
  ): Unit = {
    dut.io.flush.poke(false.B)
    dut.clock.step(4)

    // ----------------------
    // without bubble
    // ----------------------

    // write data to memory
    fork {
      dut.io.store_req.enqueueSeq(input_store_req_seq)
    }.fork {
      dut.io.store_commit.enqueueSeq(
        Seq.fill(input_store_req_seq.size)(true.B)
      )
    }.joinAndStep(dut.clock)
    // ensure all data is written to memory
    dut.clock.step(20)

    // read data from memory, and check equality
    fork {
      dut.io.load_req.enqueueSeq(input_load_req_seq)
    }.fork {
      dut.io.load_resp.expectDequeueSeq(expect_load_resp_seq)
    }.joinAndStep(dut.clock)
    dut.clock.step(20)
  }

  private def StoreQueueTestWithBubble(
      dut: StoreQueueDut,
      input_store_req_seq_size8: Seq[StoreQueueIn],
      input_load_req_seq_size8: Seq[LoadQueueIn],
      expect_load_resp_seq_size8: Seq[LoadWriteBack]
  ): Unit = {
    fork {
      input_store_req_seq_size8.foreach(input => {
        dut.io.store_req.enqueue(input)
        dut.clock.step(Gen.chooseNum(2, 10).sample.get)
      })
    }.fork {
      input_store_req_seq_size8.indices
        .foreach(_ => {
          dut.io.store_commit.enqueue(true.B)
          dut.clock.step(Gen.chooseNum(4, 10).sample.get)
        })
    }.joinAndStep(dut.clock)

    // ensure all data is written to memory
    dut.clock.step(100)

    // read data from memory, and check equality
    fork {
      input_load_req_seq_size8.foreach(load_req => {
        dut.io.load_req.enqueue(load_req)
        dut.clock.step(Gen.chooseNum(2, 10).sample.get)
      })
    }.fork {
      expect_load_resp_seq_size8.foreach(load_resp => {
        dut.io.load_resp.expectDequeue(load_resp)
        dut.clock.step(Gen.chooseNum(2, 10).sample.get)
      })
    }.joinAndStep(dut.clock)
    dut.clock.step(20)
  }

  "StoreQueueTest_size8_back_to_back" in {
    test(
      new StoreQueueDut(
      )
    ).withAnnotations(
      Seq(
        VerilatorBackendAnnotation,
        WriteFstAnnotation
      )
    ) { dut =>
      valid_ready_init(dut)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size8)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size8, req.paddr.litValue.toInt)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.wdata.litValue.toLong,
            req.size.litValue.toInt,
            req.paddr.litValue.toInt
          )
        )
      }
      // ----------------------
      // start test
      // ----------------------
      StoreQueueTestBackToBack(
        dut,
        input_store_req_seq,
        input_load_req_seq,
        expect_load_resp_seq
      )
    }
  }

  private def valid_ready_init(dut: StoreQueueDut) = {
    dut.io.store_commit.initSource().setSourceClock(dut.clock)
    dut.io.store_req.initSource().setSourceClock(dut.clock)
    dut.io.load_req.initSource().setSourceClock(dut.clock)
    dut.io.load_resp.initSink().setSinkClock(dut.clock)
    dut.io.mmio_commit.initSource().setSourceClock(dut.clock)
  }

  "StoreQueueTest_size8_with_bubble" in {
    test(
      new StoreQueueDut(
      )
    ).withAnnotations(
      Seq(
        VerilatorBackendAnnotation,
        WriteFstAnnotation
      )
    ) { dut =>
      valid_ready_init(dut)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size8)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size8, req.paddr.litValue.toInt)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.wdata.litValue.toLong,
            req.size.litValue.toInt,
            req.paddr.litValue.toInt
          )
        )
      }

      // ----------------------
      // start test
      // ----------------------
      StoreQueueTestBackToBack(
        dut,
        input_store_req_seq,
        input_load_req_seq,
        expect_load_resp_seq
      )
    }
  }

  "StoreQueueTest_size4_back_to_back" in {
    test(
      new StoreQueueDut(
      )
    ).withAnnotations(
      Seq(
        VerilatorBackendAnnotation,
        WriteFstAnnotation
      )
    ) { dut =>
      valid_ready_init(dut)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size4)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size4, req.paddr.litValue.toInt)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.wdata.litValue.toLong,
            req.size.litValue.toInt,
            req.paddr.litValue.toInt
          )
        )
      }
      // ----------------------
      // start test
      // ----------------------
      StoreQueueTestBackToBack(
        dut,
        input_store_req_seq,
        input_load_req_seq,
        expect_load_resp_seq
      )
    }
  }

  "StoreQueueTest_size4_with_bubble" in {
    test(
      new StoreQueueDut(
      )
    ).withAnnotations(
      Seq(
        VerilatorBackendAnnotation,
        WriteFstAnnotation
      )
    ) { dut =>
      valid_ready_init(dut)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size4)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size4, req.paddr.litValue.toInt)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.wdata.litValue.toLong,
            req.size.litValue.toInt,
            req.paddr.litValue.toInt
          )
        )
      }

      // ----------------------
      // start test
      // ----------------------
      StoreQueueTestWithBubble(
        dut,
        input_store_req_seq,
        input_load_req_seq,
        expect_load_resp_seq
      )
    }
  }

  "StoreQueueTest_size2_back_to_back" in {
    test(
      new StoreQueueDut(
      )
    ).withAnnotations(
      Seq(
        VerilatorBackendAnnotation,
        WriteFstAnnotation
      )
    ) { dut =>
      valid_ready_init(dut)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size2)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size2, req.paddr.litValue.toInt)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.wdata.litValue.toLong,
            req.size.litValue.toInt,
            req.paddr.litValue.toInt
          )
        )
      }
      // ----------------------
      // start test
      // ----------------------
      StoreQueueTestBackToBack(
        dut,
        input_store_req_seq,
        input_load_req_seq,
        expect_load_resp_seq
      )
    }
  }
  "StoreQueueTest_size2_with_bubble" in {
    test(
      new StoreQueueDut(
      )
    ).withAnnotations(
      Seq(
        VerilatorBackendAnnotation,
        WriteFstAnnotation
      )
    ) { dut =>
      valid_ready_init(dut)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size2)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size2, req.paddr.litValue.toInt)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.wdata.litValue.toLong,
            req.size.litValue.toInt,
            req.paddr.litValue.toInt
          )
        )
      }

      // ----------------------
      // start test
      // ----------------------
      StoreQueueTestWithBubble(
        dut,
        input_store_req_seq,
        input_load_req_seq,
        expect_load_resp_seq
      )
    }
  }
  "StoreQueueTest_size1_back_to_back" in {
    test(
      new StoreQueueDut(
      )
    ).withAnnotations(
      Seq(
        VerilatorBackendAnnotation,
        WriteFstAnnotation
      )
    ) { dut =>
      valid_ready_init(dut)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size1)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size1, req.paddr.litValue.toInt)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.wdata.litValue.toLong,
            req.size.litValue.toInt,
            req.paddr.litValue.toInt
          )
        )
      }
      // ----------------------
      // start test
      // ----------------------
      StoreQueueTestBackToBack(
        dut,
        input_store_req_seq,
        input_load_req_seq,
        expect_load_resp_seq
      )
    }
  }

  "StoreQueueTest_size1_with_bubble" in {
    test(
      new StoreQueueDut(
      )
    ).withAnnotations(
      Seq(
        VerilatorBackendAnnotation,
        WriteFstAnnotation
      )
    ) { dut =>
      valid_ready_init(dut)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size1)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size1, req.paddr.litValue.toInt)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.wdata.litValue.toLong,
            req.size.litValue.toInt,
            req.paddr.litValue.toInt
          )
        )
      }

      // ----------------------
      // start test
      // ----------------------
      StoreQueueTestWithBubble(
        dut,
        input_store_req_seq,
        input_load_req_seq,
        expect_load_resp_seq
      )
    }
  }

  "StoreQueueTest_bypass_test" in {
    test(
      new StoreQueue(
      )
    ).withAnnotations(
      Seq(
        VerilatorBackendAnnotation,
        WriteFstAnnotation
      )
    ) { dut =>
      // ----------------------
      // port init
      // ----------------------
      store_queue_port_init(dut)

      // -----------------------------------
      // prepare input and expect sequence
      // -----------------------------------
      val store_seq = 0
        .until(8)
        .map(i => {
          gen_store_req(i * 8, Gen.long.sample.get, size8)
        })

      val bypass_data_seq = store_seq.map(req => {
        (new StoreBypassData).Lit(
          _.wstrb -> req.wstrb,
          _.wdata -> req.wdata,
          _.is_mmio -> req.is_mmio,
          _.valid -> true.B
        )
      })
      val no_bypass_data = default_store_bypass()

      // ----------------------
      // start test
      // ----------------------
      dut.clock.step(5)

      // full store queue
      dut.io.dcache_req.ready.poke(false.B)
      dut.io.dcache_resp.valid.poke(false.B)
      fork {
        dut.io.in.enqueueSeq(store_seq)
      }.fork {
        dut.io.store_commit.enqueueSeq(Seq.fill(4)(true.B))
      }.joinAndStep(dut.clock)
      dut.clock.step(5)

      // all paddr should bypass
      bypass_data_seq.zipWithIndex.foreach { case (bypass_data, i) =>
        timescope {
          val paddr = Gen.chooseNum(i * 8, i * 8 + 7).sample.get
          dut.io.store_bypass.paddr.poke(paddr.U)
          dut.io.store_bypass.valid.poke(true.B)
          dut.io.store_bypass.data.expect(bypass_data)
          dut.clock.step(1)
        }
      }
      dut.clock.step(2)
      // all paddr should not bypass
      8.until(16)
        .foreach(i => {
          val paddr = Gen.chooseNum(i * 8, i * 8 + 7).sample.get
          dut.io.store_bypass.paddr.poke(paddr.U)
          dut.io.store_bypass.valid.poke(true.B)
          dut.io.store_bypass.data.expect(no_bypass_data)
          dut.clock.step(1)
        })

      pop_store_queue(dut)

      // 0-7 should not bypass
      timescope {
        val paddr = Gen.chooseNum(0, 7).sample.get
        dut.io.store_bypass.paddr.poke(paddr.U)
        dut.io.store_bypass.valid.poke(true.B)
        dut.io.store_bypass.data.expect(no_bypass_data)
        dut.clock.step(1)
      }
      dut.clock.step(2)
      // 8-15 should bypass
      timescope {
        val paddr = Gen.chooseNum(8, 15).sample.get
        dut.io.store_bypass.paddr.poke(paddr.U)
        dut.io.store_bypass.valid.poke(true.B)
        dut.io.store_bypass.data.expect(bypass_data_seq(1))
        dut.clock.step(1)
      }
      pop_store_queue(dut)
      pop_store_queue(dut)

      // 0-23 should not bypass
      timescope {
        for (i <- 0 until 24) {
          dut.io.store_bypass.paddr.poke(i.U)
          dut.io.store_bypass.valid.poke(true.B)
          dut.io.store_bypass.data.expect(no_bypass_data)
          dut.clock.step(1)
        }
      }
      // 24-63 should bypass
      timescope {
        for (i <- 24 until 64) {
          dut.io.store_bypass.paddr.poke(i.U)
          dut.io.store_bypass.valid.poke(true.B)
          dut.io.store_bypass.data.expect(bypass_data_seq(i / 8))
          dut.clock.step(1)
        }
      }

    }
  }
  "StoreQueueTest_flush_success" in {
    test(
      new StoreQueue(
      )
    ).withAnnotations(
      Seq(
        VerilatorBackendAnnotation,
        WriteFstAnnotation
      )
    ) { dut =>
      // ----------------------
      // port init
      // ----------------------
      store_queue_port_init(dut)
      // -----------------------------------
      // prepare input and expect sequence
      // -----------------------------------
      val store_seq = 8
        .until(16)
        .map(i => {
          gen_store_req(i * 8, Gen.long.sample.get, size8)
        })

      val bypass_data_seq = store_seq.map(req => {
        (new StoreBypassData).Lit(
          _.wstrb -> req.wstrb,
          _.wdata -> req.wdata,
          _.is_mmio -> req.is_mmio,
          _.valid -> true.B
        )
      })
      val no_bypass_data = default_store_bypass()

      // ----------------------
      // start test
      // ----------------------
      dut.clock.step(5)

      // full store queue
      dut.io.dcache_req.ready.poke(false.B)
      dut.io.dcache_resp.valid.poke(false.B)
      fork {
        dut.io.in.enqueueSeq(store_seq)
      }.fork {
        dut.io.store_commit.enqueueSeq(Seq.fill(4)(true.B))
      }.joinAndStep(dut.clock)
      dut.clock.step(5)

      // flush
      timescope {
        dut.io.flush.poke(true.B)
        dut.clock.step(1)
      }

      // 64-95 should  bypass
      timescope {
        for (i <- 64 until 96) {
          dut.io.store_bypass.paddr.poke(i.U)
          dut.io.store_bypass.valid.poke(true.B)
          dut.io.store_bypass.data.expect(bypass_data_seq((i - 64) / 8))

          dut.clock.step(1)
        }
      }
      // 96-127 should  not bypass
      timescope {
        for (i <- 96 until 128) {
          dut.io.store_bypass.paddr.poke(i.U)
          dut.io.store_bypass.valid.poke(true.B)
          dut.io.store_bypass.data.expect(no_bypass_data)
          dut.clock.step(1)
        }
      }

      // flush and enqueue doing at the same time
      // enqueue will be discard
      fork {
        timescope {
          dut.io.flush.poke(true.B)
          dut.clock.step(1)
        }
      }.fork {
        dut.io.in.enqueue(gen_store_req(200, Gen.long.sample.get, size8))
      }.joinAndStep(dut.clock)

      timescope {
        for (i <- 200 until 208) {
          dut.io.store_bypass.paddr.poke(i.U)
          dut.io.store_bypass.valid.poke(true.B)
          dut.io.store_bypass.data.expect(no_bypass_data)
          dut.clock.step(1)
        }
      }

    }
  }

  "StoreQueueTest_flush_panic" in {
    assertThrows[chiseltest.ChiselAssertionError] {
      test(
        new StoreQueue(
        )
      ).withAnnotations(
        Seq(
          VerilatorBackendAnnotation,
          WriteFstAnnotation
        )
      ) { dut =>
        // ----------------------
        // port init
        // ----------------------
        store_queue_port_init(dut)

        dut.clock.step(5)
        fork {
          timescope {
            dut.io.flush.poke(true.B)
            dut.clock.step(1)
          }
        }.fork {
          dut.io.store_commit.enqueue(true.B)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)
      }
    }
  }

  private def store_queue_port_init(dut: StoreQueue): Unit = {
    dut.io.store_commit.initSource().setSourceClock(dut.clock)
    dut.io.dcache_req.initSink().setSinkClock(dut.clock)
    dut.io.dcache_resp.initSource().setSourceClock(dut.clock)
    dut.io.in.initSource().setSourceClock(dut.clock)
    dut.io.flush.poke(false.B)
    dut.io.store_bypass.valid.poke(false.B)
    dut.io.store_bypass.paddr.poke(0.U)
  }
}
