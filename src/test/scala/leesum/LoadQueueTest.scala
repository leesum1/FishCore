package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.Decoupled
import chiseltest._
import leesum.Cache.{LoadDcacheReq, LoadDcacheResp}
import leesum.TestUtils.long2UInt64
import leesum.lsu.{LoadQueue, LoadQueueIn, LoadWriteBack, StoreBypassData}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

import java.nio.file.{Files, Paths}

class LoadQueueDut(memoryFile: String) extends Module {
  val io = IO(new Bundle {
    val load_req = Flipped(Decoupled(new LoadQueueIn))
    val load_resp = Decoupled(new LoadWriteBack)
    val flush = Input(Bool())
    // from commit stage, when commit a mmio instruction, set mmio_commit to true
    val mmio_commit = Flipped(Decoupled(Bool()))
  })
  val load_queue = Module(new LoadQueue())
  val dcache = Module(new DummyDCacheDut(memoryFile))

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

  def get_real_size(size: Int) = {
    require(size >= 0 && size <= 3, "size must be in range [0, 3]")
    size match {
      case 0 => 1
      case 1 => 2
      case 2 => 4
      case 3 => 8
    }
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

  /** Generate a sequence of load responses by composing response data based on
    * load requests.
    *
    * @param load_req_seq
    *   A sequence containing load requests
    * @return
    *   A sequence containing load responses
    */
  def gen_load_resp_seq(load_req_seq: IndexedSeq[LoadQueueIn]) = {
    // Process each load request
    load_req_seq.map(req => {
      // Extract the physical address from the load request and compute the starting address
      val sliceAddr = req.paddr.litValue.toInt
      val sliceAddrStart = sliceAddr - sliceAddr % 8

      // Slice out a byte sequence of 8 bytes from the memory
      val byteSeq = mem.slice(sliceAddrStart, sliceAddrStart + 8)

      // Convert the byte sequence to a 64-bit long integer
      val dataLong = TestUtils.byteSeq2Uint64LittleEndian(byteSeq)

      // Check if data composition is required based on the store_bypass signal in the load request
      val composedData = if (req.store_bypass.valid.litToBoolean) {
        // Call the compose_data function to compose data
        compose_data(
          dataLong,
          req.store_bypass.wstrb.litValue.toInt,
          req.store_bypass.wdata.litValue.toLong
        )
      } else {
        // Keep the original data unchanged
        dataLong
      }

      // Generate the load response
      gen_load_resp(composedData, req)
    })
  }

  def gen_load_req(
      paddr: Int,
      size: Int,
      trans_id: Int,
      sign_ext: Boolean = false,
      is_mmio: Boolean = false,
      store_bypass: StoreBypassData = gen_store_bypass()
  ) = {
    require(TestUtils.check_aligned(paddr, size), "paddr must be aligned")
    (new LoadQueueIn).Lit(
      _.paddr -> paddr.U,
      _.size -> size.U,
      _.is_mmio -> is_mmio.B,
      _.trans_id -> TestUtils.int2UInt32(trans_id),
      _.sign_ext -> sign_ext.B,
      _.store_bypass -> store_bypass
    )
  }

  /** Generate a load response based on response data and a load request.
    *
    * @param rdata
    *   The response data as a BigInt
    * @param load_req
    *   The load request containing necessary information
    * @return
    *   A LoadWriteBack instance representing the load response
    */
  def gen_load_resp(rdata: BigInt, load_req: LoadQueueIn) = {
    // Determine the input width based on the size of the load request
    val input_width_by_bit = get_real_size(load_req.size.litValue.toInt) * 8

    // Calculate the offset within the data based on the least significant 3 bits of the address
    val offset = load_req.paddr.litValue.toInt & 0x7

    // Sign-extend the response data based on the offset and sign extension flag
    val data_sign_ext =
      TestUtils.sign_ext(
        rdata.toLong >>> (offset * 8),
        input_width_by_bit,
        load_req.sign_ext.litToBoolean
      )

    // Create a LoadWriteBack instance with the response data and transaction ID
    (new LoadWriteBack).Lit(
      _.rdata -> TestUtils.long2UInt64(data_sign_ext),
      _.tran_id -> load_req.trans_id
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
      // TODO: not support exception
      _.exception.valid -> false.B,
      _.exception.tval -> 0.U,
      _.exception.cause -> ExceptionCause.load_access
    )
  }

  /** Compose data based on write strobe (wstrb) and write data (wdata).
    *
    * @param data
    *   The original data.
    * @param wstrb
    *   The write strobe.
    * @param wdata
    *   The write data.
    * @return
    *   The composed data.
    */
  def compose_data(data: Long, wstrb: Int, wdata: Long): Long = {
    // Convert a long value to a byte sequence
    def longToBytes(value: Long): Seq[Byte] = {
      (0 until 8).map { shift => ((value >> (shift * 8)) & 0xff).toByte }
    }

    // Convert wdata and data to little-endian byte sequences
    val wdata_byte_seq = longToBytes(wdata)
    val data_byte_seq = longToBytes(data)

    // Convert wstrb to a little-endian byte sequence
    val wstrb_byte_seq = Seq((wstrb & 0xff).toByte)

    // Compose data_byte_seq_new based on wstrb
    val data_byte_seq_new = data_byte_seq.zipWithIndex.map {
      case (byte, index) =>
        if ((wstrb_byte_seq.head & (1 << index)) != 0) {
          wdata_byte_seq(index)
        } else {
          byte
        }
    }
    // Convert the composed byte sequence back to a 64-bit little-endian integer
    val composed_data = data_byte_seq_new.zipWithIndex.foldLeft(0L) {
      case (result, (byte, index)) =>
        result | ((byte & 0xff).toLong << (index * 8))
    }
    composed_data
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
        val output_seq_size8 = gen_load_resp_seq(input_seq_size8)

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

        val output_seq_size8 = gen_load_resp_seq(input_seq_size8)
        val output_seq_size4 = gen_load_resp_seq(input_seq_size4)
        val output_seq_size2 = gen_load_resp_seq(input_seq_size2)
        val output_seq_size1 = gen_load_resp_seq(input_seq_size1)

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

  "LoadQueue_store_bypass" in {
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

        dut.io.flush.poke(false.B)
        // ---------------------
        // prepare test data
        // ---------------------

        def gen_load_req_seq(size: Int, count: Int) = {
          val real_size = get_real_size(size)
          val req_seq = 0
            .until(count)
            .map { i =>
              val store_bypass = gen_store_bypass(
                valid = Gen.prob(0.7).sample.get,
                wdata = Gen.long.sample.get,
                wstrb = (Gen.long.sample.get & 0xffL).toInt,
                is_mmio = false
              )
              gen_load_req(
                i * real_size,
                size,
                i * 10,
                sign_ext = Gen.prob(0.5).sample.get,
                false,
                store_bypass
              )
            }
          req_seq
        }

        val req_seq_size8 = gen_load_req_seq(size8, 100)
        val resp_seq_size8 = gen_load_resp_seq(req_seq_size8)
        val req_seq_size4 = gen_load_req_seq(size4, 100)
        val resp_seq_size4 = gen_load_resp_seq(req_seq_size4)

        val req_seq_size2 = gen_load_req_seq(size2, 100)
        val resp_seq_size2 = gen_load_resp_seq(req_seq_size2)
        val req_seq_size1 = gen_load_req_seq(size1, 100)
        val resp_seq_size1 = gen_load_resp_seq(req_seq_size1)

        // ---------------------
        // without bubble
        // ---------------------
        dut.io.mmio_commit.valid.poke(false.B)
        fork {
          dut.io.load_req.enqueueSeq(req_seq_size8)
          dut.io.load_req.enqueueSeq(req_seq_size4)
          dut.io.load_req.enqueueSeq(req_seq_size2)
          dut.io.load_req.enqueueSeq(req_seq_size1)
        }.fork {
          dut.io.load_resp.expectDequeueSeq(resp_seq_size8)
          dut.io.load_resp.expectDequeueSeq(resp_seq_size4)
          dut.io.load_resp.expectDequeueSeq(resp_seq_size2)
          dut.io.load_resp.expectDequeueSeq(resp_seq_size1)
        }.joinAndStep(dut.clock)

        dut.clock.step(4)
        // ---------------------
        // with bubble
        // ---------------------
        fork {
          (req_seq_size8)
            .foreach(req => {
              dut.io.load_req.enqueue(req)
              dut.clock.step(Gen.chooseNum(1, 6).sample.get)
            })
        }.fork {
          (resp_seq_size8)
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

        val load_resp_ref_seq = gen_load_resp_seq(load_req_seq)

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

        val output_seq_size8 = gen_load_resp_seq(input_seq_size8)
        val output_seq_size4 = gen_load_resp_seq(input_seq_size4)
        val output_seq_size2 = gen_load_resp_seq(input_seq_size2)
        val output_seq_size1 = gen_load_resp_seq(input_seq_size1)

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
