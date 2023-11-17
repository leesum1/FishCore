//package leesum
//
//import chisel3._
//import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
//import chisel3.util.Decoupled
//import chiseltest._
//import leesum.TestUtils.long2UInt64
//import leesum.axi4.AXI4Memory
//import org.scalacheck.Gen
//import org.scalatest.freespec.AnyFreeSpec
//
//import java.nio.file.{Files, Paths}
//
//class FetchStageTestDut(memoryFile: String = "") extends Module {
//  val io = IO(new Bundle {
//    val pc_in = Flipped(Decoupled(UInt(64.W)))
//    val fetch_resp = Decoupled(new FetchResp)
//    val flush = Input(Bool())
//  })
//
//  val fetch_stage = Module(new FetchStage)
//  val tlb = Module(new DummyTLB(random_latency = false))
//  val icache = Module(new DummyICache)
//  val axm_mem = Module(
//    new AXI4Memory(32, 64, 2048, 64, 0, memoryFile)
//  )
//
//  fetch_stage.io.flush := io.flush
//  tlb.io.flush := io.flush
//  icache.io.flush := io.flush
//
//  icache.io.axi_mem <> axm_mem.io
//  fetch_stage.io.pc_in <> io.pc_in
//  fetch_stage.io.tlb_req <> tlb.io.tlb_req
//  fetch_stage.io.tlb_resp <> tlb.io.tlb_resp
//  fetch_stage.io.icache_req <> icache.io.load_req
//  fetch_stage.io.icache_resp <> icache.io.load_resp
//  fetch_stage.io.fetch_resp <> io.fetch_resp
//}
//
//object gen_fetch_stage_dut_verilog extends App {
//  GenVerilogHelper(
//    new FetchStageTestDut()
//  )
//}
//
//class FetchStageTest extends AnyFreeSpec with ChiselScalatestTester {
//
//  val memfile = "src/main/resources/random_file.bin"
//  val byteArray = Files.readAllBytes(Paths.get(memfile))
//  val mem: Seq[Byte] = byteArray.toSeq
//
//  val mem_size8 = mem.size - mem.size % 8
//  val mem_size4 = mem.size - mem.size % 4
//  val mem_size2 = mem.size - mem.size % 2
//  val mem_size1 = mem.size - mem.size % 1
//
//  val size1 = 0
//  val size2 = 1
//  val size4 = 2
//  val size8 = 3
//
//  def ref_read(addr: Long): Long = {
//    val addr_aligned = addr - addr % 8
//    val data = mem.slice(addr_aligned.toInt, addr_aligned.toInt + 8)
//    TestUtils.byteSeq2Uint64LittleEndian(data)
//  }
//
//  def gen_fetch_resp(pc: BigInt, data: BigInt) = {
//    (new FetchResp).Lit(
//      _.pc -> long2UInt64(pc.toLong),
//      _.data -> long2UInt64(data.toLong),
//      _.exception.valid -> false.B,
//      _.exception.tval -> 0.U,
//      _.exception.cause -> ExceptionCause.misaligned_fetch
//    )
//  }
//
//  "FetchStageDataPath" in {
//    test(new FetchStageTestDut(memoryFile = memfile)).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//    ) { dut =>
//      // -----------------------
//      // init port
//      // -----------------------
//      init_port(dut)
//
//      // -----------------------
//      // prepare test data
//      // -----------------------
//
//      val pc_seq =
//        Gen
//          .listOfN(1000, Gen.chooseNum(0L, 500L))
//          .sample
//          .get
//          .map(i => TestUtils.long2UInt64(i * 2))
//
//      val fetch_resp_seq =
//        pc_seq.map(pc =>
//          gen_fetch_resp(pc.litValue, ref_read(pc.litValue.toLong))
//        )
//
//      // -----------------------
//      // test without bubble
//      // -----------------------
//      fork {
//        dut.io.pc_in.enqueueSeq(pc_seq)
//      }.fork {
//        fetch_resp_seq.foreach(resp => {
//          dut.io.fetch_resp.expectDequeue(resp)
//        })
//
//      }.joinAndStep(dut.clock)
//      dut.clock.step(4)
//
//      // -----------------------
//      // test with bubble
//      // -----------------------
//      fork {
//        pc_seq.foreach(pc => {
//          dut.io.pc_in.enqueue(pc)
//          dut.clock.step(Gen.choose(1, 10).sample.get)
//        })
//      }.fork {
//        fetch_resp_seq.foreach(resp => {
//          dut.io.fetch_resp.expectDequeue(resp)
//          dut.clock.step(Gen.choose(1, 10).sample.get)
//        })
//
//      }.joinAndStep(dut.clock)
//    }
//  }
//
//  "FetchStageFlush" in {
//    test(new FetchStage).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
//    ) { dut =>
//      // -----------------------
//      // init port
//      // -----------------------
//      dut.io.pc_in.initSource().setSourceClock(dut.clock)
//      dut.io.icache_resp.initSource().setSourceClock(dut.clock)
//      dut.io.tlb_resp.initSource().setSourceClock(dut.clock)
//      dut.io.fetch_resp.initSink().setSinkClock(dut.clock)
//      dut.io.icache_req.initSink().setSinkClock(dut.clock)
//      dut.io.tlb_req.initSink().setSinkClock(dut.clock)
//      dut.io.flush.poke(false.B)
//      dut.clock.step(5)
//
//      // -----------------------
//      // prepare test data
//      // -----------------------
//
//    }
//  }
//  private def init_port(dut: FetchStageTestDut): Unit = {
//    dut.io.pc_in.initSource().setSourceClock(dut.clock)
//    dut.io.fetch_resp.initSink().setSinkClock(dut.clock)
//    dut.io.flush.poke(false.B)
//    dut.clock.step(4)
//  }
//}
