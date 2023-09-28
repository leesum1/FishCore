package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.Decoupled
import chiseltest._
import leesum.TestUtils.long2UInt64
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

import java.nio.file.{Files, Paths}
import scala.io.Source

class LoadQueueDut(memoryFile: String) extends Module {
  val io = IO(new Bundle {
    val load_req = Flipped(Decoupled(new LoadQueueIn))
    val load_resp = Decoupled(new LoadWriteBack)
    val flush = Input(Bool())
    // from commit stage, when commit a mmio instruction, set mmio_commit to true
    val mmio_commit = Flipped(Decoupled(Bool()))
  })
  val load_queue = Module(new LoadQueue())
  val dcache = Module(new DummyDCache(memoryFile))

  load_queue.io.in <> io.load_req
  load_queue.io.load_wb <> io.load_resp
  load_queue.io.flush := io.flush
  load_queue.io.mmio_commit <> io.mmio_commit

  load_queue.io.dcache_req <> dcache.io.load_req
  load_queue.io.dcache_resp <> dcache.io.load_resp
  dcache.io.flush := io.flush
  dcache.io.store_req.valid := false.B
  dcache.io.store_req.bits.size := 0.U
  dcache.io.store_req.bits.paddr := 0.U
  dcache.io.store_req.bits.wdata := 0.U
  dcache.io.store_req.bits.wstrb := 0.U
  dcache.io.store_req.bits.is_mmio := false.B
  dcache.io.store_resp.ready := false.B
}

object gen_load_queue_dut_verilog extends App {
  GenVerilogHelper(new LoadQueueDut(""))
}

class LoadQueueTest extends AnyFreeSpec with ChiselScalatestTester {

  val memfile = "src/main/resources/random_file.bin"
  val byteArray = Files.readAllBytes(Paths.get(memfile))
  val mem: Seq[Byte] = byteArray.toSeq

  val mem_size8 = mem.size - mem.size % 8
  val mem_size4 = mem.size - mem.size % 4
  val mem_size2 = mem.size - mem.size % 2
  val mem_size1 = mem.size - mem.size % 1

  val size1 = 0
  val size2 = 1
  val size4 = 2
  val size8 = 3

  def gen_store_bypass() = {
    (new StoreBypassData).Lit(
      _.wstrb -> 0.U,
      _.wdata -> 0.U,
      _.is_mmio -> false.B,
      _.valid -> false.B
    )
  }

  def gen_load_req(
      paddr: Int,
      size: Int,
      trans_id: Int,
      sign_ext: Boolean = false,
      is_mmio: Boolean = false
  ) = {
    require(TestUtils.check_aligned(paddr, size), "paddr must be aligned")
    (new LoadQueueIn).Lit(
      _.paddr -> paddr.U,
      _.size -> size.U,
      // TODO: not implement is_mmio
      _.is_mmio -> is_mmio.B,
      _.trans_id -> TestUtils.int2UInt32(trans_id),
      // TODO: not implement sign_ext
      _.sign_ext -> sign_ext.B,
      // TODO: not implement store_bypass
      _.store_bypass -> gen_store_bypass()
    )
  }

  def gen_load_resp(rdata: BigInt, trans_id: BigInt) = {
    (new LoadWriteBack).Lit(
      _.rdata -> TestUtils.long2UInt64(rdata.toLong),
      _.tran_id -> TestUtils.int2UInt32(trans_id.toInt)
    )

  }

  def gen_load_dcache_req(
      paddr: UInt,
      size: UInt,
      is_mmio: Boolean = false
  ): LoadDcacheReq = {
    (new LoadDcacheReq).Lit(
      _.paddr -> paddr,
      _.size -> size,
      _.is_mmio -> is_mmio.B
    )
  }

  def gen_load_dcache_resp(data: Long): LoadDcacheResp = {
    (new LoadDcacheResp).Lit(
      _.data -> long2UInt64(data),
      _.exception.valid -> false.B,
      _.exception.tval -> 0.U,
      _.exception.cause -> ExceptionCause.load_access
    )
  }

  "LoadQueueTest_mmio" in {
    test(
      new LoadQueueDut(
        memoryFile = memfile
      )
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ---------------------
        // init port
        // ---------------------
        dut.io.load_req.initSource().setSourceClock(dut.clock)
        dut.io.load_resp.initSink().setSinkClock(dut.clock)
        dut.io.mmio_commit.initSource().setSourceClock(dut.clock)

        // ---------------------
        // prepare test data
        // ---------------------
        val input_seq_size8 = 0
          .until(mem_size8, 8)
          .map { addr =>
            gen_load_req(addr, size8, addr, sign_ext = true, is_mmio = true)
          }
        val output_seq_size8 = input_seq_size8.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 8)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          val data_sign_ext =
            TestUtils.sign_ext(data_long, 64, req.sign_ext.litToBoolean)
          gen_load_resp(data_sign_ext, req.trans_id.litValue)
        })

        dut.clock.step(4)

        // ---------------------
        // without bubble
        // ---------------------
        fork {
          dut.io.load_req.enqueueSeq(input_seq_size8)
        }.fork {
          dut.io.load_resp.expectDequeueSeq(output_seq_size8)
        }.fork {
          dut.io.mmio_commit.enqueueSeq(
            Seq.fill(input_seq_size8.size)(true.B)
          )
        }.joinAndStep(dut.clock)

        // ---------------------
        // with bubble
        // ---------------------
        fork {
          input_seq_size8.foreach { req =>
            dut.io.load_req.enqueue(req)
            dut.clock.step(Gen.chooseNum(1, 6).sample.get)
          }
        }.fork {
          output_seq_size8.foreach { output =>
            dut.io.load_resp.expectDequeue(output)
            dut.clock.step(Gen.chooseNum(1, 14).sample.get)
          }
        }.fork {
          input_seq_size8.foreach { _ =>
            dut.io.mmio_commit.enqueue(true.B)
            dut.clock.step(Gen.chooseNum(1, 6).sample.get)
          }
        }.joinAndStep(dut.clock)

      }
  }

  "LoadQueueTest_sign_ext" in {
    test(
      new LoadQueueDut(
        memoryFile = memfile
      )
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ---------------------
        // init port
        // ---------------------
        dut.io.load_req.initSource().setSourceClock(dut.clock)
        dut.io.load_resp.initSink().setSinkClock(dut.clock)
        dut.io.mmio_commit.initSource().setSourceClock(dut.clock)
        // ---------------------
        // prepare test data
        // ---------------------
        val input_seq_size8 = 0
          .until(mem_size8, 8)
          .map { addr =>
            gen_load_req(addr, size8, addr, sign_ext = true)
          }
        val input_seq_size4 = 0
          .until(mem_size4, 4)
          .map { addr =>
            gen_load_req(addr, size4, addr, sign_ext = true)
          }
        val input_seq_size2 = 0
          .until(mem_size2, 2)
          .map { addr =>
            gen_load_req(addr, size2, addr, sign_ext = true)
          }
        val input_seq_size1 = 0
          .until(mem_size1, 1)
          .map { addr =>
            gen_load_req(addr, size1, addr, sign_ext = true)
          }

        val output_seq_size8 = input_seq_size8.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 8)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          val data_sign_ext =
            TestUtils.sign_ext(data_long, 64, req.sign_ext.litToBoolean)
          gen_load_resp(data_sign_ext, req.trans_id.litValue)
        })
        val output_seq_size4 = input_seq_size4.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 4)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          val data_sign_ext =
            TestUtils.sign_ext(data_long, 32, req.sign_ext.litToBoolean)
          gen_load_resp(data_sign_ext, req.trans_id.litValue)
        })
        val output_seq_size2 = input_seq_size2.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 2)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          val data_sign_ext =
            TestUtils.sign_ext(data_long, 16, req.sign_ext.litToBoolean)
          gen_load_resp(data_sign_ext, req.trans_id.litValue)
        })
        val output_seq_size1 = input_seq_size1.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 1)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          val data_sign_ext =
            TestUtils.sign_ext(data_long, 8, req.sign_ext.litToBoolean)
          gen_load_resp(data_sign_ext, req.trans_id.litValue)
        })

        dut.clock.step(4)
        // ---------------------
        // without bubble
        // ---------------------
        dut.io.mmio_commit.valid.poke(false.B)
        fork {
          dut.io.load_req.enqueueSeq(input_seq_size8)
          dut.io.load_req.enqueueSeq(input_seq_size4)
          dut.io.load_req.enqueueSeq(input_seq_size2)
          dut.io.load_req.enqueueSeq(input_seq_size1)
        }.fork {
          dut.io.load_resp.expectDequeueSeq(output_seq_size8)
          dut.io.load_resp.expectDequeueSeq(output_seq_size4)
          dut.io.load_resp.expectDequeueSeq(output_seq_size2)
          dut.io.load_resp.expectDequeueSeq(output_seq_size1)
        }.joinAndStep(dut.clock)

        // ---------------------
        // with bubble
        // ---------------------

        fork {
          (input_seq_size1 ++ input_seq_size2 ++ input_seq_size4 ++ input_seq_size8)
            .foreach(req => {
              dut.io.load_req.enqueue(req)
              dut.clock.step(Gen.chooseNum(1, 6).sample.get)
            })
        }.fork {
          (output_seq_size1 ++ output_seq_size2 ++ output_seq_size4 ++ output_seq_size8)
            .foreach(output => {
              dut.io.load_resp.expectDequeue(output)
              dut.clock.step(Gen.chooseNum(1, 14).sample.get)
            })
        }.joinAndStep(dut.clock)
      }
  }

  "LoadQueueTest_flush" in {
    test(
      new LoadQueue()
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ---------------------
        // init port
        // ---------------------
        dut.io.in.initSource().setSourceClock(dut.clock)
        dut.io.load_wb.initSink().setSinkClock(dut.clock)
        dut.io.dcache_req.initSink().setSinkClock(dut.clock)
        dut.io.dcache_resp.initSource().setSourceClock(dut.clock)

        dut.io.mmio_commit.initSource().setSourceClock(dut.clock)
        dut.io.flush.poke(false.B)

        // ---------------------
        // prepare test data
        // ---------------------
        val load_req_seq = 0
          .until(32, 8)
          .map { addr =>
            gen_load_req(addr, size8, addr * 10)
          }

        val load_dcache_req_seq = load_req_seq.map(req => {
          gen_load_dcache_req(req.paddr, req.size, req.is_mmio.litToBoolean)
        })

        val load_dcache_resp_seq = load_req_seq.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 8)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          gen_load_dcache_resp(data_long)
        })

        val load_resp_ref_seq = load_req_seq.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 8)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          gen_load_resp(data_long, req.trans_id.litValue)
        })

        def after_flush_test(
            dut: LoadQueue,
            load_req_seq: IndexedSeq[LoadQueueIn],
            load_dcache_req_seq: IndexedSeq[LoadDcacheReq],
            load_dcache_resp_seq: IndexedSeq[LoadDcacheResp],
            load_resp_ref_seq: IndexedSeq[LoadWriteBack]
        ): Unit = {
          dut.io.flush.poke(false.B)
          dut.clock.step(4)

          fork {
            dut.io.in.enqueueSeq(load_req_seq)
          }.fork {
            load_dcache_req_seq
              .zip(load_dcache_resp_seq)
              .foreach({
                case (req, resp) => {
                  dut.io.dcache_req.expectDequeue(req)
                  dut.io.dcache_resp.enqueue(resp)
                }
              })
          }.fork {
            dut.io.load_wb.expectDequeueSeq(load_resp_ref_seq)
          }.joinAndStep(dut.clock)
          dut.clock.step(4)
        }

        // -----------------------------------------------
        // 1. dcache_resp and flush arrival at the same time
        // -----------------------------------------------
        dut.io.flush.poke(false.B)
        // send first load_req
        dut.io.in.enqueue(load_req_seq.head)

        // accept first dcache_req
        dut.io.dcache_req.expectDequeue(load_dcache_req_seq.head)

        // 1. dcache_resp and flush valid at the same time
        fork {
          dut.io.dcache_resp.enqueue(load_dcache_resp_seq.head)
        }.fork {
          timescope {
            dut.io.flush.poke(true.B)
            dut.clock.step(1)
          }
        }.joinAndStep(dut.clock)

        after_flush_test(
          dut,
          load_req_seq,
          load_dcache_req_seq,
          load_dcache_resp_seq,
          load_resp_ref_seq
        )
        // -----------------------------------------------
        // 2. dcache_resp later than flush signal
        // -----------------------------------------------
        dut.io.flush.poke(false.B)
        // send first load_req
        dut.io.in.enqueue(load_req_seq.head)

        // accept first dcache_req
        dut.io.dcache_req.expectDequeue(load_dcache_req_seq.head)

        // 2. dcache_resp later than flush signal
        fork {
          dut.clock.step(5)
          dut.io.dcache_resp.enqueue(load_dcache_resp_seq.head)
        }.fork {
          timescope {
            dut.io.flush.poke(true.B)
            dut.clock.step(1)
          }
        }.joinAndStep(dut.clock)

        after_flush_test(
          dut,
          load_req_seq,
          load_dcache_req_seq,
          load_dcache_resp_seq,
          load_resp_ref_seq
        )
        // -----------------------------------------------
        // 3. dcache_resp earlier than flush signal
        // -----------------------------------------------
        dut.io.flush.poke(false.B)
        // send first load_req
        dut.io.in.enqueue(load_req_seq.head)

        // accept first dcache_req
        dut.io.dcache_req.expectDequeue(load_dcache_req_seq.head)

        // 3. dcache_resp earlier than flush signal
        fork {
          dut.io.dcache_resp.enqueue(load_dcache_resp_seq.head)
        }.fork {
          dut.clock.step(5)
          timescope {
            dut.io.flush.poke(true.B)
            dut.clock.step(1)
          }
        }.fork {
          dut.io.load_wb.expectDequeue(load_resp_ref_seq.head)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)
        after_flush_test(
          dut,
          load_req_seq,
          load_dcache_req_seq,
          load_dcache_resp_seq,
          load_resp_ref_seq
        )

      }
  }

  "LoadQueueTest_load_size" in {
    test(
      new LoadQueueDut(
        memoryFile = memfile
      )
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ---------------------
        // init port
        // ---------------------
        dut.io.load_req.initSource().setSourceClock(dut.clock)
        dut.io.load_resp.initSink().setSinkClock(dut.clock)
        dut.io.mmio_commit.initSource().setSourceClock(dut.clock)
        // ---------------------
        // prepare test data
        // ---------------------
        val input_seq_size8 = 0
          .until(mem_size8, 8)
          .map { addr =>
            gen_load_req(addr, size8, addr)
          }

        val input_seq_size4 = 0
          .until(mem_size4, 4)
          .map { addr =>
            gen_load_req(addr, size4, addr)
          }
        val input_seq_size2 = 0
          .until(mem_size2, 2)
          .map { addr =>
            gen_load_req(addr, size2, addr)
          }
        val input_seq_size1 = 0
          .until(mem_size1, 1)
          .map { addr =>
            gen_load_req(addr, size1, addr)
          }

        val output_seq_size8 = input_seq_size8.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 8)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          gen_load_resp(data_long, req.trans_id.litValue)
        })
        val output_seq_size4 = input_seq_size4.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 4)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          gen_load_resp(data_long, req.trans_id.litValue)
        })
        val output_seq_size2 = input_seq_size2.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 2)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          gen_load_resp(data_long, req.trans_id.litValue)
        })
        val output_seq_size1 = input_seq_size1.map(req => {
          val byte_seq =
            mem.slice(req.paddr.litValue.toInt, req.paddr.litValue.toInt + 1)
          val data_long = TestUtils.byteSeq2Uint64LittleEndian(byte_seq)
          gen_load_resp(data_long, req.trans_id.litValue)
        })

        dut.clock.step(4)
        // ---------------------
        // without bubble
        // ---------------------
        dut.io.mmio_commit.valid.poke(false.B)
        fork {
          dut.io.load_req.enqueueSeq(input_seq_size8)
          dut.io.load_req.enqueueSeq(input_seq_size4)
          dut.io.load_req.enqueueSeq(input_seq_size2)
          dut.io.load_req.enqueueSeq(input_seq_size1)
        }.fork {
          dut.io.load_resp.expectDequeueSeq(output_seq_size8)
          dut.io.load_resp.expectDequeueSeq(output_seq_size4)
          dut.io.load_resp.expectDequeueSeq(output_seq_size2)
          dut.io.load_resp.expectDequeueSeq(output_seq_size1)
        }.joinAndStep(dut.clock)

        dut.clock.step(4)
        // ---------------------
        // with bubble
        // ---------------------
        fork {
          (input_seq_size1 ++ input_seq_size2 ++ input_seq_size4 ++ input_seq_size8)
            .foreach(req => {
              dut.io.load_req.enqueue(req)
              dut.clock.step(Gen.chooseNum(1, 6).sample.get)
            })
        }.fork {
          (output_seq_size1 ++ output_seq_size2 ++ output_seq_size4 ++ output_seq_size8)
            .foreach(output => {
              dut.io.load_resp.expectDequeue(output)
              dut.clock.step(Gen.chooseNum(1, 14).sample.get)
            })
        }.joinAndStep(dut.clock)

      }
  }

}
