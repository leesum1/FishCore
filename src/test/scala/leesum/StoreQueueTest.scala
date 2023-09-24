package leesum

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.Decoupled
import chiseltest._
import leesum.test_utils.long2UInt64
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class StoreQueueDut extends Module {

  val io = IO(new Bundle {
    val store_req = Flipped(Decoupled(new StoreQueueIn))
    val store_commit = Flipped(Decoupled(Bool()))
    val load_req = Flipped(Decoupled(new LoadDcacheReq))
    val load_resp = Decoupled(new LoadDcacheResp)

    val flush = Input(Bool())
  })
  val store_queue = Module(new StoreQueue())
  val dcache = Module(new DummyDCache())

  store_queue.io.in <> io.store_req
  store_queue.io.store_commit <> io.store_commit
  store_queue.io.flush := io.flush
  dcache.io.flush := io.flush

  store_queue.io.dcache_req <> dcache.io.store_req
  store_queue.io.dcache_resp <> dcache.io.store_resp

  dcache.io.load_req <> io.load_req
  io.load_resp <> dcache.io.load_resp
}

object gen_store_queue_dut_verilog extends App {
  GenVerilogHelper(new StoreQueueDut())
}

class StoreQueueTest extends AnyFreeSpec with ChiselScalatestTester {

  val size1 = 0
  val size2 = 1
  val size4 = 2
  val size8 = 3

  def gen_store_req(
      paddr: Int,
      store_data: Long,
      size: Int,
      is_mmio: Boolean = false
  ) = {
    require(test_utils.check_aligned(paddr, size), "paddr must be aligned")
    (new StoreQueueIn).Lit(
      _.paddr -> paddr.U,
      _.size -> size.U,
      _.store_data -> long2UInt64(store_data),
      _.is_mmio -> is_mmio.B
    )
  }

  def gen_load_req(paddr: Int, size: Int, is_mmio: Boolean = false) = {
    require(test_utils.check_aligned(paddr, size), "paddr must be aligned")
    (new LoadDcacheReq).Lit(
      _.paddr -> paddr.U,
      _.size -> size.U,
      _.is_mmio -> is_mmio.B
    )
  }

  def gen_load_resp(addr: BigInt, data: Long, size: Int) = {
    val offset = ((addr % 8) * 8).toLong
    val data_shifted = data >> offset

    val data_masked = size match {
      case 0 => data_shifted & 0xffL
      case 1 => data_shifted & 0xffffL
      case 2 =>
        data_shifted & 0xffffffffL
      case 3 => data_shifted
    }

    (new LoadDcacheResp).Lit(
      _.data -> long2UInt64(data_masked),
      _.exception -> (new ExceptionEntry)
        .Lit(
          _.tval -> 0.U,
          _.valid -> false.B,
          _.cause -> ExceptionCause.load_access
        )
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

  private def StoreQueueTestBackToBack(
      dut: StoreQueueDut,
      input_store_req_seq: Seq[StoreQueueIn],
      input_load_req_seq: Seq[LoadDcacheReq],
      expect_load_resp_seq: Seq[LoadDcacheResp]
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
      input_load_req_seq_size8: Seq[LoadDcacheReq],
      expect_load_resp_seq_size8: Seq[LoadDcacheResp]
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
      dut.io.store_commit.initSource().setSourceClock(dut.clock)
      dut.io.store_req.initSource().setSourceClock(dut.clock)
      dut.io.load_req.initSource().setSourceClock(dut.clock)
      dut.io.load_resp.initSink().setSinkClock(dut.clock)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size8)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size8)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.store_data.litValue.toLong,
            req.size.litValue.toInt
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
      dut.io.store_commit.initSource().setSourceClock(dut.clock)
      dut.io.store_req.initSource().setSourceClock(dut.clock)
      dut.io.load_req.initSource().setSourceClock(dut.clock)
      dut.io.load_resp.initSink().setSinkClock(dut.clock)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size8)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size8)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.store_data.litValue.toLong,
            req.size.litValue.toInt
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
      dut.io.store_commit.initSource().setSourceClock(dut.clock)
      dut.io.store_req.initSource().setSourceClock(dut.clock)
      dut.io.load_req.initSource().setSourceClock(dut.clock)
      dut.io.load_resp.initSink().setSinkClock(dut.clock)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size4)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size4)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.store_data.litValue.toLong,
            req.size.litValue.toInt
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
      dut.io.store_commit.initSource().setSourceClock(dut.clock)
      dut.io.store_req.initSource().setSourceClock(dut.clock)
      dut.io.load_req.initSource().setSourceClock(dut.clock)
      dut.io.load_resp.initSink().setSinkClock(dut.clock)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size4)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size4)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.store_data.litValue.toLong,
            req.size.litValue.toInt
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
      dut.io.store_commit.initSource().setSourceClock(dut.clock)
      dut.io.store_req.initSource().setSourceClock(dut.clock)
      dut.io.load_req.initSource().setSourceClock(dut.clock)
      dut.io.load_resp.initSink().setSinkClock(dut.clock)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size2)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size2)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.store_data.litValue.toLong,
            req.size.litValue.toInt
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
      dut.io.store_commit.initSource().setSourceClock(dut.clock)
      dut.io.store_req.initSource().setSourceClock(dut.clock)
      dut.io.load_req.initSource().setSourceClock(dut.clock)
      dut.io.load_resp.initSink().setSinkClock(dut.clock)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size2)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size2)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.store_data.litValue.toLong,
            req.size.litValue.toInt
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
      dut.io.store_commit.initSource().setSourceClock(dut.clock)
      dut.io.store_req.initSource().setSourceClock(dut.clock)
      dut.io.load_req.initSource().setSourceClock(dut.clock)
      dut.io.load_resp.initSink().setSinkClock(dut.clock)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size1)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size1)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.store_data.litValue.toLong,
            req.size.litValue.toInt
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
      dut.io.store_commit.initSource().setSourceClock(dut.clock)
      dut.io.store_req.initSource().setSourceClock(dut.clock)
      dut.io.load_req.initSource().setSourceClock(dut.clock)
      dut.io.load_resp.initSink().setSinkClock(dut.clock)

      // ----------------------------------
      // prepare input and expect sequence
      // ----------------------------------
      val input_store_req_seq = gen_store_req_input_seq(size1)
      val input_load_req_seq = input_store_req_seq.map(req => {
        gen_load_req(req.paddr.litValue.toInt, size1)
      })
      val expect_load_resp_seq = {
        input_store_req_seq.map(req =>
          gen_load_resp(
            req.paddr.litValue,
            req.store_data.litValue.toLong,
            req.size.litValue.toInt
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

}
