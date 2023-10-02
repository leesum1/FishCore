package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.Decoupled
import chiseltest._
import leesum.TestUtils.{
  check_aligned,
  gen_axi_wdata,
  gen_axi_wstrb,
  gen_rand_uint,
  long2UInt64
}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

import scala.language.postfixOps

class AGU_dut extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AGUReq))
    val out = new AGUResp
    val store_bypass = Flipped(new StoreBypassIO)
    val flush = Input(Bool())
  })

  val agu = Module(new AGU)
  val tlb = Module(new DummyTLB)

  agu.io.in <> io.in
  agu.io.out <> io.out
  agu.io.tlb_req <> tlb.io.tlb_req
  agu.io.tlb_resp <> tlb.io.tlb_resp
  agu.io.store_bypass <> io.store_bypass

  tlb.io.flush := io.flush
  agu.io.flush := io.flush
}
class AGU_dut2 extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AGUReq))
    val out = new AGUResp
    val flush = Input(Bool())
    // tlb interface
    val tlb_req = Decoupled(new TLBReq)
    val tlb_resp = Flipped(Decoupled(new TLBResp))
    // from store bypass
    val store_bypass = Flipped(new StoreBypassIO)
  })

  val agu = Module(new AGU)

  io.out.store_pipe <> PipeLine(agu.io.out.store_pipe, io.flush)
  io.out.load_pipe <> PipeLine(agu.io.out.load_pipe, io.flush)
  io.out.exception_pipe <> PipeLine(agu.io.out.exception_pipe, io.flush)

  agu.io.in <> io.in
  agu.io.tlb_req <> io.tlb_req
  agu.io.tlb_resp <> io.tlb_resp
  agu.io.store_bypass <> io.store_bypass
  agu.io.flush := io.flush
}

object gen_agu_dut_verilog extends App {
  GenVerilogHelper(new AGU_dut2())
}

class AGUTest extends AnyFreeSpec with ChiselScalatestTester {
  val size1 = 0
  val size2 = 1
  val size4 = 2
  val size8 = 3

  def gen_agu_in(addr: Long, is_store: Boolean, size: Int): AGUReq = {
    (new AGUReq).Lit(
      _.op_a -> long2UInt64(addr),
      _.op_b -> 0.U,
      _.size -> size.U,
      _.store_data -> gen_rand_uint(64).sample.get,
      _.is_store -> is_store.B,
      _.trans_id -> gen_rand_uint(32).sample.get,
      _.sign_ext -> false.B
    )
  }

  def gen_agu_in_input(): Gen[AGUReq] = {
    val op_a = gen_rand_uint(64)
    val op_b = gen_rand_uint(64)
    val size = gen_rand_uint(2)
    val store_data = gen_rand_uint(64)
    val trans_id = gen_rand_uint(32)
    val is_store = Gen.oneOf(true.B, false.B)
    val sign_ext = Gen.oneOf(true.B, false.B)

    val input_gen = for {
      op_a <- op_a
      op_b <- op_b
      size <- size
      store_data <- store_data
      is_store <- is_store
      trans_id <- trans_id
      sign_ext <- sign_ext
    } yield {
      (new AGUReq).Lit(
        _.op_a -> op_a,
        _.op_b -> 0.U,
        _.size -> size,
        _.store_data -> store_data,
        _.is_store -> is_store,
        _.trans_id -> trans_id,
        _.sign_ext -> sign_ext
      )
    }
    input_gen
  }

  def gen_store_bypass(
      valid: Boolean = false,
      wdata: Long = 0,
      wstrb: Int = 0,
      is_mmio: Boolean = false
  ) = {
    (new StoreBypassData).Lit(
      _.wstrb -> wstrb.U,
      _.wdata -> long2UInt64(wdata),
      _.is_mmio -> is_mmio.B,
      _.valid -> valid.B
    )
  }

  "AGU_dispatch_test" in {
    test(new AGU_dut)
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.in.initSource().setSourceClock(dut.clock)
        dut.io.out.load_pipe.initSink().setSinkClock(dut.clock)
        dut.io.out.store_pipe.initSink().setSinkClock(dut.clock)
        dut.io.out.exception_pipe.initSink().setSinkClock(dut.clock)

        dut.io.store_bypass.data.valid.poke(false.B)
        dut.io.store_bypass.data.is_mmio.poke(false.B)
        dut.io.store_bypass.data.wdata.poke(0.U)
        dut.io.store_bypass.data.wstrb.poke(0.U)

        dut.clock.step(5)

        val default_bypass_data = gen_store_bypass()

        val input_seq = Gen.listOfN(1000, gen_agu_in_input()).sample.get

        // if addr is aligned, then generate load or store
        val output_load_seq = input_seq
          .filter(input => {
            !input.is_store.litToBoolean && check_aligned(
              (input.op_a.litValue + input.op_b.litValue).toLong,
              input.size.litValue.toInt
            )
          })
          .map(input => {
            gen_load_queue_entry(input, default_bypass_data)
          })
        // if addr is aligned, then generate load or store
        val output_store_seq = input_seq
          .filter(input => {
            input.is_store.litToBoolean && check_aligned(
              (input.op_a.litValue + input.op_b.litValue).toLong,
              input.size.litValue.toInt
            )
          })
          .map(input => {
            gen_store_queue_entry(input)
          })

        // if addr is not aligned, then generate exception
        val output_exception_seq = input_seq
          .filter(input => {
            !check_aligned(
              (input.op_a.litValue + input.op_b.litValue).toLong,
              input.size.litValue.toInt
            )
          })
          .map(input => {
            gen_exception_queue_entry(input)
          })

        dut.clock.step(5)
        // ---------------------------
        // back to back,no bubble
        // ---------------------------
        fork {
          dut.io.in.enqueueSeq(input_seq)
        }.fork {
          dut.io.out.load_pipe.expectDequeueSeq(output_load_seq)
        }.fork {
          dut.io.out.store_pipe.expectDequeueSeq(output_store_seq)
        }.fork {
          dut.io.out.exception_pipe.expectDequeueSeq(output_exception_seq)
        }.joinAndStep(dut.clock)

        dut.clock.step(5)
        // -----------------------
        // with bubble
        // -----------------------
        fork {
          input_seq.foreach(input => {
            dut.io.in.enqueue(input)
            dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }.fork {
          output_load_seq.foreach(output => {
            dut.io.out.load_pipe.expectDequeue(output)
            dut.clock.step(Gen.chooseNum(4, 15).sample.get)
          })
        }.fork {
          output_store_seq.foreach(output => {
            dut.io.out.store_pipe.expectDequeue(output)
            dut.clock.step(Gen.chooseNum(1, 4).sample.get)
          })
        }.fork {
          output_exception_seq.foreach(output => {
            dut.io.out.exception_pipe.expectDequeue(output)
            dut.clock.step(Gen.chooseNum(1, 4).sample.get)
          })
        }.joinAndStep(dut.clock)
      }
  }

  "AGU_flush_test" in {
    test(new AGU_dut2)
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // --------------------
        // init ports
        // --------------------
        dut.io.in.initSource().setSourceClock(dut.clock)
        dut.io.out.load_pipe.initSink().setSinkClock(dut.clock)
        dut.io.out.store_pipe.initSink().setSinkClock(dut.clock)
        dut.io.out.exception_pipe.initSink().setSinkClock(dut.clock)
        dut.io.tlb_req.initSink().setSinkClock(dut.clock)
        dut.io.tlb_resp.initSource().setSourceClock(dut.clock)
        dut.io.flush.poke(false.B)
        dut.io.store_bypass.data.valid.poke(false.B)
        dut.io.store_bypass.data.is_mmio.poke(false.B)
        dut.io.store_bypass.data.wdata.poke(0.U)
        dut.io.store_bypass.data.wstrb.poke(0.U)

        val default_bypass_data = gen_store_bypass()
        // --------------------
        // prepare test data
        // --------------------
        val input_seq = Gen.listOfN(10, gen_agu_in_input()).sample.get
        val tlb_req_seq = input_seq.map(agu_req => {
          gen_tlb_req(agu_req)
        })
        val tlb_resp_seq = tlb_req_seq.map(tlb_req => {
          gen_tlb_resp(tlb_req)
        })

        val out_seq: List[Any] = input_seq.map(agu_req => {
          gen_agu_out(agu_req, default_bypass_data)
        })

        val out_seq_type = input_seq.map(agu_req => {
          val vaddr = agu_req.op_a.litValue + agu_req.op_b.litValue
          val is_store = agu_req.is_store.litToBoolean
          if (check_aligned(vaddr.toLong, agu_req.size.litValue.toInt)) {
            if (is_store) {
              "store"
            } else {
              "load"
            }
          } else {
            "exception"
          }
        })
        // -----------------------------------------------------
        // 1. flush signal and tlb_resp valid at the same cycle
        // -----------------------------------------------------

        // step1: send  first agu_req to AGU
        fork {
          dut.io.in.enqueue(input_seq.head)
        }.fork {
          dut.io.tlb_req.expectDequeue(tlb_req_seq.head)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)
        // step2: send flush signal and first tlb_resp
        // (flush signal and tlb_resp valid at the same cycle)
        fork {
          timescope {
            dut.io.flush.poke(true.B)
            dut.clock.step(1)
          }
        }.fork {
          // response the first agu_req
          dut.io.tlb_resp.enqueue(tlb_resp_seq.head)
        }.joinAndStep(dut.clock)

        // step3: after flush test
        after_flush_test(
          dut,
          input_seq.tail,
          tlb_req_seq.tail,
          tlb_resp_seq.tail,
          out_seq.tail,
          out_seq_type.tail
        )

        // -----------------------------------------------------
        // 2. flush signal earlier than tlb_resp valid
        // -----------------------------------------------------
        // step1: send  first agu_req to AGU
        fork {
          dut.io.in.enqueue(input_seq(1))
        }.fork {
          dut.io.tlb_req.expectDequeue(tlb_req_seq(1))
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        // step2: send flush signal and first tlb_resp
        // (flush signal earlier than tlb_resp valid)
        fork {
          timescope {
            dut.io.flush.poke(true.B)
            dut.clock.step(1)
          }
        }.fork {
          dut.clock.step(Gen.chooseNum(4, 10).sample.get)
          // response the first agu_req
          dut.io.tlb_resp.enqueue(tlb_resp_seq(1))
        }.joinAndStep(dut.clock)

        // step3: after flush test
        after_flush_test(
          dut,
          input_seq.drop(2),
          tlb_req_seq.drop(2),
          tlb_resp_seq.drop(2),
          out_seq.drop(2),
          out_seq_type.drop(2)
        )
        // -----------------------------------------------------
        // 3. flush signal later than tlb_resp valid
        // -----------------------------------------------------
        // step1: send first agu_req to AGU
        fork {
          dut.io.in.enqueue(input_seq(3))
        }.fork {
          dut.io.tlb_req.expectDequeue(tlb_req_seq(3))
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        // step2: send flush signal and  first tlb_resp
        // (flush signal later than tlb_resp valid)
        fork {
          dut.clock.step(Gen.chooseNum(4, 10).sample.get)
          timescope {
            dut.io.flush.poke(true.B)
            dut.clock.step(1)
          }
        }.fork {
          // response the first agu_req
          dut.io.tlb_resp.enqueue(tlb_resp_seq(3))
        }.joinAndStep(dut.clock)

        // step3: after flush test
        after_flush_test(
          dut,
          input_seq,
          tlb_req_seq,
          tlb_resp_seq,
          out_seq,
          out_seq_type
        )

      }
  }

  private def gen_tlb_req(agu_req: AGUReq): TLBReq = {
    val vaddr = agu_req.op_a.litValue + agu_req.op_b.litValue
    val req_type =
      if (agu_req.is_store.litToBoolean) TLBReqType.STORE
      else TLBReqType.LOAD
    (new TLBReq).Lit(
      _.size -> agu_req.size,
      _.vaddr -> TestUtils.long2UInt64(vaddr.toLong),
      _.req_type -> req_type
    )
  }

  private def gen_agu_out(
      agu_req: AGUReq,
      bypass_data: StoreBypassData
  ): Any = {
    val vaddr = agu_req.op_a.litValue + agu_req.op_b.litValue
    val is_store = agu_req.is_store.litToBoolean
    if (check_aligned(vaddr.toLong, agu_req.size.litValue.toInt)) {
      if (is_store) {
        gen_store_queue_entry(agu_req)
      } else {
        gen_load_queue_entry(agu_req, bypass_data)
      }
    } else {
      gen_exception_queue_entry(agu_req)
    }
  }

  private def gen_tlb_resp(tlb_req: TLBReq) = {
    val cause = if (tlb_req.req_type == TLBReqType.STORE) {
      ExceptionCause.store_access
    } else {
      ExceptionCause.load_access
    }
    (new TLBResp).Lit(
      _.paddr -> tlb_req.vaddr,
      _.req_type -> tlb_req.req_type,
      _.size -> tlb_req.size,
      _.exception.valid -> false.B,
      _.exception.tval -> 0.U,
      _.exception.cause -> cause
    )
  }

  "AGU_store_bypass_test" in {
    test(new AGU_dut2)
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // --------------------
        // init ports
        // --------------------
        dut.io.in.initSource().setSourceClock(dut.clock)
        dut.io.out.load_pipe.initSink().setSinkClock(dut.clock)
        dut.io.out.store_pipe.initSink().setSinkClock(dut.clock)
        dut.io.out.exception_pipe.initSink().setSinkClock(dut.clock)
        dut.io.tlb_req.initSink().setSinkClock(dut.clock)
        dut.io.tlb_resp.initSource().setSourceClock(dut.clock)
        dut.io.flush.poke(false.B)
        dut.io.store_bypass.data.valid.poke(false.B)
        dut.io.store_bypass.data.is_mmio.poke(false.B)
        dut.io.store_bypass.data.wdata.poke(0.U)
        dut.io.store_bypass.data.wstrb.poke(0.U)

        // --------------------
        // prepare input data
        // --------------------

        def bypass_data(addr: Long, use_mmio: Boolean) = {
          val wdata_seq = {
            Seq(
              0x1234567887654321L,
              0x8765432144332211L,
              0xdeadbeef43214433L,
              0xabcdef01eef432b2L
            )
          }
          val wstrb_seq = Seq(0xff, 0x0f, 0x03, 0x01)
          val is_mmio_seq = Seq(true, true, false, false)

          val mmio = if (use_mmio) {
            is_mmio_seq((addr / 8).toInt)
          } else {
            false
          }
          if ((addr >= 0 && addr < 24)) {
            gen_store_bypass(
              valid = true,
              wdata = wdata_seq((addr / 8).toInt),
              wstrb = wstrb_seq((addr / 8).toInt),
              is_mmio = mmio
            )
          } else {
            gen_store_bypass()
          }
        }

        val input_store_seq = 0
          .until(32, 4)
          .map(
            gen_agu_in(_, is_store = true, size4)
          )
        val input_load_seq = 0
          .until(32, 4)
          .map(
            gen_agu_in(_, is_store = false, size4)
          )
        val input_ex_seq = 0
          .until(32, 2)
          .filter(!check_aligned(_, size2))
          .map(
            gen_agu_in(_, Gen.oneOf(true, false).sample.get, size2)
          )
        val store_tlb_req_seq = input_store_seq.map(gen_tlb_req)
        val load_tlb_req_seq = input_load_seq.map(gen_tlb_req)
        val store_tlb_resp_seq = store_tlb_req_seq.map(gen_tlb_resp)
        val load_tlb_resp_seq = load_tlb_req_seq.map(gen_tlb_resp)
        val store_out_seq = input_store_seq.map(gen_store_queue_entry)
        val load_out_seq_with_bypass = input_load_seq.map(in => {
          gen_load_queue_entry(
            in,
            bypass_data(
              (in.op_a.litValue + in.op_b.litValue).toLong,
              use_mmio = false
            )
          )
        })
        val load_out_seq_without_bypass = input_load_seq.map(in => {
          gen_load_queue_entry(
            in,
            gen_store_bypass()
          )
        })

        // --------------------
        // load stall test1
        // --------------------
        dut.clock.step(4)
        // init store_bypass data
        dut.io.store_bypass.data.poke(
          bypass_data(
            load_tlb_resp_seq.head.paddr.litValue.toLong,
            use_mmio = true
          )
        )
        // send load request and tlb request
        fork {
          dut.io.in.enqueue(input_load_seq.head)
        }.fork {
          dut.io.tlb_req.expectDequeue(load_tlb_req_seq.head)
        }.joinAndStep(dut.clock)

        // get tlb response
        dut.clock.step(4)
        fork {
          dut.io.tlb_resp.enqueue(load_tlb_resp_seq.head)
        }.joinAndStep(dut.clock)

        // expect load response, when store_bypass.data.valid is true and store_bypass.data.is_mmio is true
        // load response will be stall
        fork {
          dut.io.out.load_pipe.expectDequeue(load_out_seq_with_bypass.head)
        }
          .fork {
            dut.clock.step(5)
            // stall until store_bypass.data.valid is false
            dut.io.store_bypass.data.is_mmio.poke(false.B)
            dut.clock.step(2)
          }
          .joinAndStep(dut.clock)
        dut.clock.step(4)

        // --------------------
        // load stall test2
        // --------------------
        dut.clock.step(4)
        // init store_bypass data
        dut.io.store_bypass.data.poke(
          bypass_data(
            load_tlb_resp_seq(1).paddr.litValue.toLong,
            use_mmio = true
          )
        )
        // send load request and tlb request
        fork {
          dut.io.in.enqueue(input_load_seq(1))
        }.fork {
          dut.io.tlb_req.expectDequeue(load_tlb_req_seq(1))
        }.joinAndStep(dut.clock)

        // get tlb response
        dut.clock.step(4)
        fork {
          dut.io.tlb_resp.enqueue(load_tlb_resp_seq(1))
        }.joinAndStep(dut.clock)

        // expect load response, when store_bypass.data.valid is true and store_bypass.data.is_mmio is true
        // load response will be stall
        fork {
          dut.io.out.load_pipe.expectDequeue(load_out_seq_without_bypass(1))
        }
          .fork {
            dut.clock.step(5)
            // stall until store_bypass.data.valid is false
            dut.io.store_bypass.data.valid.poke(false.B)
            dut.clock.step(2)
          }
          .joinAndStep(dut.clock)
        dut.clock.step(4)

        // --------------------
        // store stall
        // --------------------
        fork {
          dut.io.in.enqueue(input_store_seq.head)
        }.fork {
          dut.io.tlb_req.expectDequeue(store_tlb_req_seq.head)
        }.joinAndStep(dut.clock)

        dut.clock.step(4)
        fork {
          dut.io.tlb_resp.enqueue(store_tlb_resp_seq.head)
        }.joinAndStep(dut.clock)

        fork {
          dut.io.out.store_pipe.expectDequeue(store_out_seq.head)
        }
          .fork {
            dut.clock.step(5)
            // store stall until store_bypass.data.valid is false
            dut.io.store_bypass.data.valid.poke(false.B)
            dut.clock.step(2)
          }
          .joinAndStep(dut.clock)
        dut.clock.step(4)
      }

  }
  private def after_flush_test(
      dut: AGU_dut2,
      input_seq: List[AGUReq],
      tlb_req_seq: List[TLBReq],
      tlb_resp_seq: List[TLBResp],
      out_seq: List[Any],
      out_seq_type: List[String]
  ): Unit = {
    fork {
      input_seq.foreach(input => {
        dut.io.in.enqueue(input)
        dut.clock.step(Gen.chooseNum(1, 10).sample.get)
      })
    }.fork {
      tlb_req_seq.foreach(tlb_req => {
        dut.io.tlb_req.expectDequeue(tlb_req)
        dut.clock.step(Gen.chooseNum(1, 10).sample.get)
      })
    }.fork {
      tlb_resp_seq.foreach(tlb_resp => {
        dut.io.tlb_resp.enqueue(tlb_resp)
        dut.clock.step(Gen.chooseNum(1, 10).sample.get)
      })
    }.fork {
      out_seq.zip(out_seq_type).foreach { case (out_data, out_data_type) =>
        expect_agu_out(dut, out_data, out_data_type)
        dut.clock.step(Gen.chooseNum(1, 10).sample.get)
      }
    }.joinAndStep(dut.clock)
    dut.clock.step(5)
  }

  private def expect_agu_out(
      dut: AGU_dut2,
      out_data: Any,
      out_data_type: String
  ): Unit = {
    if (out_data_type == "exception") {
      dut.io.out.exception_pipe.expectDequeue(
        out_data.asInstanceOf[ExceptionQueueIn]
      )
    } else if (out_data_type == "load") {
      dut.io.out.load_pipe.expectDequeue(
        out_data.asInstanceOf[LoadQueueIn]
      )
    } else if (out_data_type == "store") {
      dut.io.out.store_pipe.expectDequeue(
        out_data.asInstanceOf[StoreQueueIn]
      )
    }
  }

  private def gen_load_queue_entry(
      agu_req: AGUReq,
      bypass_data: StoreBypassData
  ) = {
    (new LoadQueueIn).Lit(
      _.paddr -> long2UInt64(
        (agu_req.op_a.litValue + agu_req.op_b.litValue).toLong
      ),
      _.size -> agu_req.size,
      _.is_mmio -> false.B,
      _.trans_id -> agu_req.trans_id,
      _.sign_ext -> agu_req.sign_ext,
      _.store_bypass -> bypass_data
    )
  }

  private def gen_exception_queue_entry(input: AGUReq) = {
    val ex = (new ExceptionEntry).Lit(
      _.valid -> true.B,
      _.tval -> long2UInt64(
        (input.op_a.litValue + input.op_b.litValue).toLong
      ),
      _.cause -> (if (input.is_store.litToBoolean) {
                    ExceptionCause.misaligned_store
                  } else {
                    ExceptionCause.misaligned_load
                  })
    )

    (new ExceptionQueueIn).Lit(
      _.trans_id -> input.trans_id,
      _.exception -> ex,
      _.is_mmio -> false.B
    )
  }

  private def gen_store_queue_entry(input: AGUReq) = {
    val paddr = long2UInt64(
      (input.op_a.litValue + input.op_b.litValue).toLong
    )
    val wdata = long2UInt64(
      gen_axi_wdata(
        input.store_data.litValue.toLong,
        paddr.litValue.toLong
      )
    )
    val wstrb =
      gen_axi_wstrb(paddr.litValue.toLong, input.size.litValue.toInt).U

    (new StoreQueueIn).Lit(
      _.paddr -> paddr,
      _.wdata -> wdata,
      _.size -> input.size,
      _.wstrb -> wstrb,
      _.is_mmio -> false.B,
      _.trans_id -> input.trans_id
    )
  }
}
