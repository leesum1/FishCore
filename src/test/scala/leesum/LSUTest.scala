//package leesum
//import chisel3._
//import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
//import chisel3.util.Decoupled
//import chiseltest._
//import leesum.TestUtils.{check_aligned, int2UInt32, long2UInt64}
//import org.scalacheck.Gen
//import org.scalatest.freespec.AnyFreeSpec
//
//import java.util.concurrent.LinkedTransferQueue
//import java.nio.file.{Files, Paths}
//import scala.jdk.CollectionConverters.IterableHasAsJava
//
//class LSUTestDut(memoryFIle: String = "") extends Module {
//  val io = IO(new Bundle {
//    val lsu_req = Flipped(Decoupled(new LSUReq))
//    val flush = Input(Bool())
//    // commit interface
//    val mmio_commit = Flipped(Decoupled(Bool()))
//    val store_commit = Flipped(Decoupled(Bool()))
//    // write-back interface
//    val lsu_resp = Decoupled(new LSUResp)
//  })
//
//  val lsu = Module(new LSU)
//  val dcache = Module(new DummyDCacheDut(memoryFIle))
//  val tlb = Module(new DummyTLB())
//
//  // flush
//  lsu.io.flush := io.flush
//  dcache.io.flush := io.flush
//  tlb.io.flush := io.flush
//
//  // lsu <> io
//  lsu.io.lsu_req <> io.lsu_req
//  lsu.io.mmio_commit <> io.mmio_commit
//  lsu.io.store_commit <> io.store_commit
//  lsu.io.lsu_resp <> io.lsu_resp
//
//  // lsu <> dcache
//  lsu.io.dcache_load_req <> dcache.io.load_req
//  lsu.io.dcache_load_resp <> dcache.io.load_resp
//  lsu.io.dcache_store_req <> dcache.io.store_req
//  lsu.io.dcache_store_resp <> dcache.io.store_resp
//
//  // lsu <> tlb
//  lsu.io.tlb_req <> tlb.io.tlb_req
//  lsu.io.tlb_resp <> tlb.io.tlb_resp
//
//  // lsu <> lsu_resp
//  lsu.io.lsu_resp <> io.lsu_resp
//
//}
//
//object gen_LSUTestDut_verilog extends App {
//  GenVerilogHelper(new LSUTestDut)
//}
//
//class RefLSUMemory(ADDR_WIDTH: Int, DATA_WIDTH: Int, BASE_ADDR: Int) {
//  require(DATA_WIDTH % 8 == 0, "DATA_WIDTH must be a multiple of 8")
//  var mem = Array.fill(1 << ADDR_WIDTH)(0.toByte)
//
//  def get_real_size(size: BigInt): Int = {
//    size.toInt match {
//      case 0 => 1
//      case 1 => 2
//      case 2 => 4
//      case 3 => 8
//    }
//  }
//  def get_real_addr(addr: BigInt): Int = {
//    (addr - BASE_ADDR).toInt
//  }
//  def read(addr: BigInt, size: BigInt): BigInt = {
//    require(
//      check_aligned(addr.toLong, size.toInt),
//      "RefLSUMemory must be aligned"
//    )
//    val real_size = get_real_size(size)
//    val real_addr = get_real_addr(addr)
//
//    val rdata = mem
//      .slice(real_addr, real_addr + real_size)
//    TestUtils.byteSeq2Uint64LittleEndian(rdata)
//  }
//
//  def write(addr: BigInt, size: BigInt, data: BigInt): Unit = {
//    require(
//      check_aligned(addr.toLong, size.toInt),
//      "RefLSUMemory must be aligned"
//    )
//    val real_size = get_real_size(size)
//    val real_addr = get_real_addr(addr)
//
//    0.until(real_size).foreach { i =>
//      mem(real_addr + i) = ((data >> (i * 8)) & 0xff).toByte
//    }
//  }
//}
//
//object test_run1 extends App {
//  var mem = new RefLSUMemory(12, 64, 0)
//
//  mem.write(0, 3, 0x123456789abcdefL)
//
//  val byte_array = mem.read(0, 3).toByteArray
//
//}
//
//class LSUTest extends AnyFreeSpec with ChiselScalatestTester {
//  val memfile = "src/main/resources/random_file.bin"
//  val byteArray = Files.readAllBytes(Paths.get(memfile))
//  val mem: Seq[Byte] = byteArray.toSeq
//
//  val mem_size = mem.size - mem.size % 8
//
//  val size1 = 0
//  val size2 = 1
//  val size4 = 2
//  val size8 = 3
//
//  def gen_lsu_req(
//      op_a: Long,
//      op_b: Long,
//      size: Int,
//      wdata: Long,
//      trans_id: Long,
//      is_store: Boolean,
//      sign_ext: Boolean
//  ): LSUReq = {
//    (new LSUReq).Lit(
//      _.size -> size.U,
//      _.op_a -> long2UInt64(op_a),
//      _.op_b -> long2UInt64(op_b),
//      _.store_data -> long2UInt64(wdata),
//      _.is_store -> is_store.B,
//      _.sign_ext -> sign_ext.B,
//      _.trans_id -> long2UInt64(trans_id)
//    )
//  }
//  def get_real_size(size: Int): Int = {
//    require(size >= 0 && size <= 3, "size must be in [0,3]")
//    size match {
//      case 0 => 1
//      case 1 => 2
//      case 2 => 4
//      case 3 => 8
//    }
//  }
//  def gen_lsu_load_req(addr: Long, size: Int, sign_ext: Boolean): LSUReq = {
//    require(check_aligned(addr, size), "addr must be aligned")
//    gen_lsu_req(addr, 0, size, 0, addr.toInt * 10, false, sign_ext)
//  }
//
////  class LSUResp extends Bundle {
////    val trans_id = UInt(32.W)
////    val wb_data = UInt(64.W)
////    val exception = new ExceptionEntry(has_valid = true)
////  }
//  def gen_lsu_load_resp(wb_data: Long, trans_id: Int): LSUResp = {
//    (new LSUResp).Lit(
//      _.trans_id -> int2UInt32(trans_id),
//      _.wb_data -> long2UInt64(wb_data),
//      _.exception.valid -> false.B,
//      // TODO: not implement
//      _.is_mmio -> false.B,
//      _.exception.tval -> 0.U,
//      _.exception.cause -> ExceptionCause.misaligned_fetch // 0
//    )
//  }
//
//  def gen_lsu_store_req(addr: Long, size: Int, wdata: Long): LSUReq = {
//    require(check_aligned(addr, size), "addr must be aligned")
//    gen_lsu_req(addr, 0, size, wdata, addr.toInt * 10, true, false)
//  }
//
//  // TODO:gen_lsu_misaligned_req
//  def gen_lsu_misaligned_req(
//      addr: Long,
//      size: Int,
//      sign_ext: Boolean
//  ): LSUReq = {
//    require(!check_aligned(addr, size), "addr must be misaligned")
//    gen_lsu_req(addr, 0, size, 0, addr.toInt * 10, false, sign_ext)
//  }
//
//  def gen_aligned_addr(size: Int): Long = {
//    require(size >= 0 && size <= 3, "size must be in [0,3]")
//    val real_size = get_real_size(size)
//    val addr = Gen.choose(0, 1024).sample.get
//    addr - addr % real_size
//  }
//
//  def gen_lsu_random_load_req() = {
//    val size = Gen.choose(0, 3).sample.get
//    gen_lsu_load_req(
//      addr = gen_aligned_addr(size),
//      size = size,
//      sign_ext = Gen.prob(0.5).sample.get
//    )
//  }
//
//  def gen_lsu_random_write_req() = {
//    val size = Gen.choose(0, 3).sample.get
//    gen_lsu_store_req(
//      addr = gen_aligned_addr(size),
//      size = size,
//      wdata = Gen.long.sample.get
//    )
//  }
//
//  "LSU_load_req_test" in {
//    test(new LSUTestDut(memoryFIle = memfile)).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//    ) { dut =>
//      // -----------------------
//      // init port
//      // -----------------------
//      lsu_dut_init_port(dut)
//
//      // -----------------------
//      // prepare test data
//      // -----------------------
//      def gen_lsu_load_req_seq(
//          size: Int
//      ): Seq[LSUReq] = {
//        val real_size = get_real_size(size)
//        0.until(mem_size, real_size).map { addr =>
//          gen_lsu_load_req(addr, size, Gen.prob(0.5).sample.get)
//        }
//      }
//
//      def gen_lsu_load_resp_seq(
//          load_req_seq: Seq[LSUReq]
//      ): Seq[LSUResp] = {
//        load_req_seq.map { req =>
//          val real_size = get_real_size(req.size.litValue.toInt)
//          val addr = (req.op_a.litValue + req.op_b.litValue).toLong
//          val mem_data = TestUtils.byteSeq2Uint64LittleEndian(
//            mem.slice(addr.toInt, addr.toInt + real_size)
//          )
//          val data_with_sign_ext =
//            TestUtils.sign_ext(
//              mem_data,
//              real_size * 8,
//              req.sign_ext.litToBoolean
//            )
//          gen_lsu_load_resp(data_with_sign_ext, req.trans_id.litValue.toInt)
//        }
//      }
//
//      val load_req_seq = gen_lsu_load_req_seq(size1) ++
//        gen_lsu_load_req_seq(size2) ++
//        gen_lsu_load_req_seq(size4) ++
//        gen_lsu_load_req_seq(size8)
//
//      val load_resp_seq = gen_lsu_load_resp_seq(load_req_seq)
//
//      lsu_common_load_test(dut, load_req_seq, load_resp_seq)
//    }
//  }
//  "LSU_load_loopback_test" in {
//    test(new LSUTestDut()).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//    ) { dut =>
//      // -----------------------
//      // init port
//      // -----------------------
//      lsu_dut_init_port(dut)
//
//      // -----------------------
//      // prepare test data
//      // -----------------------
//      def gen_lsu_load_req_seq(
//          size: Int
//      ): Seq[LSUReq] = {
//        val real_size = get_real_size(size)
//        0.until(1024, real_size).map { addr =>
//          gen_lsu_load_req(addr, size, Gen.prob(0.5).sample.get)
//        }
//      }
//      def gen_lsu_store_req_seq(
//          size: Int
//      ): Seq[LSUReq] = {
//        val real_size = get_real_size(size)
//        0.until(1024, real_size).map { addr =>
//          gen_lsu_store_req(addr, size, Gen.long.sample.get)
//        }
//      }
//
//      def gen_lsu_load_resp_seq(
//          load_req_seq: Seq[LSUReq],
//          store_req_seq: Seq[LSUReq]
//      ): Seq[LSUResp] = {
//        require(load_req_seq.size == store_req_seq.size)
//        load_req_seq.zip(store_req_seq).map { case (load_req, store_req) =>
//          require(load_req.op_a.litValue == store_req.op_a.litValue)
//          require(load_req.op_b.litValue == store_req.op_b.litValue)
//          require(load_req.size.litValue == store_req.size.litValue)
//          val real_size = get_real_size(load_req.size.litValue.toInt)
//          val store_data = store_req.store_data.litValue
//          val data_with_sign_ext =
//            TestUtils.sign_ext(
//              store_data.toLong,
//              real_size * 8,
//              load_req.sign_ext.litToBoolean
//            )
//          gen_lsu_load_resp(
//            data_with_sign_ext,
//            load_req.trans_id.litValue.toInt
//          )
//        }
//      }
//
//      val store_req_seq_size1 = gen_lsu_store_req_seq(size1)
//      val store_req_seq_size2 = gen_lsu_store_req_seq(size2)
//      val store_req_seq_size4 = gen_lsu_store_req_seq(size4)
//      val store_req_seq_size8 = gen_lsu_store_req_seq(size8)
//
//      val load_req_seq_size1 = gen_lsu_load_req_seq(size1)
//      val load_req_seq_size2 = gen_lsu_load_req_seq(size2)
//      val load_req_seq_size4 = gen_lsu_load_req_seq(size4)
//      val load_req_seq_size8 = gen_lsu_load_req_seq(size8)
//
//      def lsu_write_mem_without_bubble(
//          store_req_seq: Seq[LSUReq]
//      ): Unit = {
//        fork {
//          dut.io.lsu_req.enqueueSeq(store_req_seq)
//        }.fork {
//          dut.io.store_commit.enqueueSeq(
//            Seq.fill(store_req_seq.size)(true.B)
//          )
//        }.joinAndStep(dut.clock)
//      }
//
//      def lsu_write_mem_with_bubble(
//          store_req_seq: Seq[LSUReq]
//      ): Unit = {
//        fork {
//          store_req_seq.foreach(req => {
//            dut.io.lsu_req.enqueue(req)
//            dut.clock.step(Gen.choose(1, 10).sample.get)
//          })
//        }.fork {
//          store_req_seq.indices.foreach { _ =>
//            dut.io.store_commit.enqueue(true.B)
//            dut.clock.step(Gen.choose(1, 10).sample.get)
//          }
//        }.joinAndStep(dut.clock)
//      }
//
//      // -------------------------------------
//      // size1 loopback test(without bubble)
//      // -------------------------------------
//      lsu_write_mem_without_bubble(store_req_seq_size1)
//      lsu_common_load_test(
//        dut,
//        load_req_seq_size1,
//        gen_lsu_load_resp_seq(load_req_seq_size1, store_req_seq_size1)
//      )
//      // -------------------------------------
//      // size2 loopback test(without bubble)
//      // -------------------------------------
//      lsu_write_mem_without_bubble(store_req_seq_size2)
//      lsu_common_load_test(
//        dut,
//        load_req_seq_size2,
//        gen_lsu_load_resp_seq(load_req_seq_size2, store_req_seq_size2)
//      )
//      // -------------------------------------
//      // size4 loopback test(without bubble)
//      // -------------------------------------
//      lsu_write_mem_without_bubble(store_req_seq_size4)
//      lsu_common_load_test(
//        dut,
//        load_req_seq_size4,
//        gen_lsu_load_resp_seq(load_req_seq_size4, store_req_seq_size4)
//      )
//      // -------------------------------------
//      // size8 loopback test(with bubble)
//      // -------------------------------------
//      lsu_write_mem_with_bubble(store_req_seq_size8)
//      lsu_common_load_test(
//        dut,
//        load_req_seq_size8,
//        gen_lsu_load_resp_seq(load_req_seq_size8, store_req_seq_size8)
//      )
//      // -------------------------------------
//      // size1 loopback test(with bubble)
//      // -------------------------------------
//      lsu_write_mem_with_bubble(store_req_seq_size1)
//      lsu_common_load_test(
//        dut,
//        load_req_seq_size1,
//        gen_lsu_load_resp_seq(load_req_seq_size1, store_req_seq_size1)
//      )
//      // -------------------------------------
//      // size2 loopback test(with bubble)
//      // -------------------------------------
//      lsu_write_mem_with_bubble(store_req_seq_size2)
//      lsu_common_load_test(
//        dut,
//        load_req_seq_size2,
//        gen_lsu_load_resp_seq(load_req_seq_size2, store_req_seq_size2)
//      )
//      // -------------------------------------
//      // size4 loopback test(with bubble)
//      // -------------------------------------
//      lsu_write_mem_with_bubble(store_req_seq_size4)
//      lsu_common_load_test(
//        dut,
//        load_req_seq_size4,
//        gen_lsu_load_resp_seq(load_req_seq_size4, store_req_seq_size4)
//      )
//      // -------------------------------------
//      // size8 loopback test(with bubble)
//      // -------------------------------------
//      lsu_write_mem_with_bubble(store_req_seq_size8)
//      lsu_common_load_test(
//        dut,
//        load_req_seq_size8,
//        gen_lsu_load_resp_seq(load_req_seq_size8, store_req_seq_size8)
//      )
//
//    }
//  }
//  "LSU_random_read_write_test" in {
//    test(new LSUTestDut()).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//    ) { dut =>
//      // -----------------------
//      // init port
//      // -----------------------
//      lsu_dut_init_port(dut)
//
//      val test_case_size = 10000
//
//      val ref_mem = new RefLSUMemory(12, 64, 0)
//      // -----------------------
//      // prepare test data
//      // -----------------------
//
//      val lsu_req_seq = 0
//        .until(test_case_size)
//        .map(_ => {
//          if (Gen.prob(0.5).sample.get) {
//            gen_lsu_random_load_req()
//          } else {
//            gen_lsu_random_write_req()
//          }
//        })
//
//      val ref_load_resp_queue = scala.collection.mutable.Queue[LSUResp]()
//
//      // calculate ref data
//      lsu_req_seq.foreach(req => {
//        val addr = req.op_a.litValue + req.op_b.litValue
//        if (req.is_store.litToBoolean) {
//          ref_mem.write(
//            addr,
//            req.size.litValue,
//            req.store_data.litValue
//          )
//        } else {
//          val rdata = ref_mem.read(addr, req.size.litValue)
//          val data_with_sign_ext =
//            TestUtils.sign_ext(
//              rdata.toLong,
//              get_real_size(req.size.litValue.toInt) * 8,
//              req.sign_ext.litToBoolean
//            )
//          val load_resp = gen_lsu_load_resp(
//            data_with_sign_ext,
//            req.trans_id.litValue.toInt
//          )
//          ref_load_resp_queue.enqueue(load_resp)
//        }
//      })
//
//      // -----------------------
//      // without bubble
//      // -----------------------
//      val lsu_order_buffer =
//        new LinkedTransferQueue[LSUReq](lsu_req_seq.asJavaCollection)
//
//      fork {
//        lsu_req_seq.foreach(req => {
//          dut.io.lsu_req.enqueue(req)
//          dut.clock.step(1)
//        })
//      }.fork {
//        while (!lsu_order_buffer.isEmpty) {
//          if (lsu_order_buffer.peek().is_store.litToBoolean) {
//            dut.io.store_commit.enqueue(true.B)
//            // retire store
//            lsu_order_buffer.take()
//          }
//          dut.clock.step(1)
//        }
//      }.fork {
//        while (!lsu_order_buffer.isEmpty) {
//          if (!lsu_order_buffer.peek().is_store.litToBoolean) {
//            dut.io.lsu_resp.expectDequeue(ref_load_resp_queue.dequeue())
//            // retire load
//            lsu_order_buffer.take()
//          }
//          dut.clock.step(1)
//        }
//      }.joinAndStep(dut.clock)
//
//      assert(ref_load_resp_queue.isEmpty)
//      assert(lsu_order_buffer.isEmpty)
//
//      dut.clock.step(5)
//      // -----------------------
//      // with bubble
//      // -----------------------
//
//      // calculate ref data
//      lsu_req_seq.foreach(req => {
//        if (req.is_store.litToBoolean) {
//          ref_mem.write(
//            req.op_a.litValue + req.op_b.litValue,
//            req.size.litValue,
//            req.store_data.litValue
//          )
//        } else {
//          val addr = req.op_a.litValue + req.op_b.litValue
//          val rdata = ref_mem.read(addr, req.size.litValue)
//          val data_with_sign_ext =
//            TestUtils.sign_ext(
//              rdata.toLong,
//              get_real_size(req.size.litValue.toInt) * 8,
//              req.sign_ext.litToBoolean
//            )
//          val load_resp = gen_lsu_load_resp(
//            data_with_sign_ext,
//            req.trans_id.litValue.toInt
//          )
//          ref_load_resp_queue.enqueue(load_resp)
//        }
//      })
//
//      val lsu_order_buffer_with_bubble =
//        new LinkedTransferQueue[LSUReq](lsu_req_seq.asJavaCollection)
//
//      fork {
//        lsu_req_seq.foreach(req => {
//          dut.io.lsu_req.enqueue(req)
//          dut.clock.step(Gen.choose(1, 6).sample.get)
//        })
//      }.fork {
//        while (!lsu_order_buffer_with_bubble.isEmpty) {
//          if (lsu_order_buffer_with_bubble.peek().is_store.litToBoolean) {
//            dut.io.store_commit.enqueue(true.B)
//            // retire store
//            lsu_order_buffer_with_bubble.take()
//          }
//          dut.clock.step(Gen.choose(1, 10).sample.get)
//        }
//      }.fork {
//        while (!lsu_order_buffer_with_bubble.isEmpty) {
//          if (!lsu_order_buffer_with_bubble.peek().is_store.litToBoolean) {
//            dut.io.lsu_resp.expectDequeue(ref_load_resp_queue.dequeue())
//            // retire load
//            lsu_order_buffer_with_bubble.take()
//          }
//          dut.clock.step(Gen.choose(1, 10).sample.get)
//        }
//      }.joinAndStep(dut.clock)
//
//      assert(ref_load_resp_queue.isEmpty)
//      assert(lsu_order_buffer_with_bubble.isEmpty)
//      dut.clock.step(5)
//
//    }
//  }
//  // TODO: not implemented mmio now
//  "LSU_flush_test" in {
//    test(new LSUTestDut()).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//    ) { dut =>
//      // -----------------------
//      // init port
//      // -----------------------
//      lsu_dut_init_port(dut)
//      val ref_mem = new RefLSUMemory(12, 64, 0)
//      val test_case_size = 5000
//      // -----------------------
//      // prepare test data
//      // -----------------------
//
//      // lsu req need to be flushed
//      val lsu_load_req_seq_before_flush = 0
//        .until(3)
//        .map(_ => {
//          gen_lsu_random_load_req()
//        })
//      val lsu_store_req_before_flush = 0
//        .until(3)
//        .map(_ => {
//          gen_lsu_random_write_req()
//        })
//
//      // lsu req don't need to be flushed
//      val lsu_req_seq = 0
//        .until(test_case_size)
//        .map(_ => {
//          if (Gen.prob(0.5).sample.get) {
//            gen_lsu_random_load_req()
//          } else {
//            gen_lsu_random_write_req()
//          }
//        })
//
//      val ref_load_resp_queue = scala.collection.mutable.Queue[LSUResp]()
//      // calculate ref data
//      lsu_req_seq.foreach(req => {
//        val addr = req.op_a.litValue + req.op_b.litValue
//        if (req.is_store.litToBoolean) {
//          ref_mem.write(
//            addr,
//            req.size.litValue,
//            req.store_data.litValue
//          )
//        } else {
//          val rdata = ref_mem.read(addr, req.size.litValue)
//          val data_with_sign_ext =
//            TestUtils.sign_ext(
//              rdata.toLong,
//              get_real_size(req.size.litValue.toInt) * 8,
//              req.sign_ext.litToBoolean
//            )
//          val load_resp = gen_lsu_load_resp(
//            data_with_sign_ext,
//            req.trans_id.litValue.toInt
//          )
//          ref_load_resp_queue.enqueue(load_resp)
//        }
//      })
//
//      val lsu_order_buffer =
//        new LinkedTransferQueue[LSUReq](lsu_req_seq.asJavaCollection)
//      // -----------------------
//      // flush test
//      // -----------------------
//      dut.io.lsu_req.enqueueSeq(lsu_load_req_seq_before_flush)
//      dut.io.lsu_req.enqueueSeq(lsu_store_req_before_flush)
//      dut.clock.step(2)
//      // flush previous lsu req
//      timescope {
//        dut.io.flush.poke(true.B)
//        dut.clock.step(1)
//      }
//
//      dut.clock.step(5)
//
//      // after flush, load queue and speculated store queue should be empty
//      fork {
//        dut.io.lsu_req.enqueueSeq(lsu_req_seq)
//      }.fork {
//        while (!lsu_order_buffer.isEmpty) {
//          if (lsu_order_buffer.peek().is_store.litToBoolean) {
//            dut.io.store_commit.enqueue(true.B)
//            // retire store
//            lsu_order_buffer.take()
//          }
//          dut.clock.step(1)
//        }
//      }.fork {
//        while (!lsu_order_buffer.isEmpty) {
//          if (!lsu_order_buffer.peek().is_store.litToBoolean) {
//            dut.io.lsu_resp.expectDequeue(ref_load_resp_queue.dequeue())
//            // retire load
//            lsu_order_buffer.take()
//          }
//          dut.clock.step(1)
//        }
//      }.joinAndStep(dut.clock)
//
//    }
//  }
//
//  // TODO: not implemented mmio now
//  "LSU_random_mmio_test" in {
//    test(new LSUTestDut()).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//    ) { dut =>
//      // -----------------------
//      // init port
//      // -----------------------
//      lsu_dut_init_port(dut)
//
//      // -----------------------
//      // prepare test data
//      // -----------------------
//    }
//  }
//
//  private def lsu_common_load_test(
//      dut: LSUTestDut,
//      load_req_seq: Seq[LSUReq],
//      load_resp_seq: Seq[LSUResp]
//  ): Unit = {
//    // -----------------------
//    // without bubble
//    // -----------------------
//    fork {
//      dut.io.lsu_req.enqueueSeq(load_req_seq)
//    }.fork {
//      dut.io.lsu_resp.expectDequeueSeq(load_resp_seq)
//    }.joinAndStep(dut.clock)
//    dut.clock.step(5)
//
//    // -----------------------
//    // with bubble
//    // -----------------------
//    fork {
//      load_req_seq.foreach(req => {
//        dut.io.lsu_req.enqueue(req)
//        dut.clock.step(Gen.choose(1, 10).sample.get)
//      })
//    }.fork {
//      load_resp_seq.foreach(resp => {
//        dut.io.lsu_resp.expectDequeue(resp)
//        dut.clock.step(Gen.choose(1, 10).sample.get)
//      })
//    }.joinAndStep(dut.clock)
//    dut.clock.step(5)
//  }
//
//  private def lsu_dut_init_port(dut: LSUTestDut): Unit = {
//    dut.io.lsu_req.initSource().setSourceClock(dut.clock)
//    dut.io.lsu_resp.initSink().setSinkClock(dut.clock)
//    dut.io.mmio_commit.initSource().setSourceClock(dut.clock)
//    dut.io.store_commit.initSource().setSourceClock(dut.clock)
//    dut.io.flush.poke(false.B)
//    dut.clock.step(5)
//  }
//}
