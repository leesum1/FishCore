package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import leesum.TestUtils.{byteSeq2Uint64LittleEndian, long2UInt64}
import leesum.axi4.AXIDef.{BURST_INCR, SIZE_4, SIZE_8}
import leesum.axi4._
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

import java.nio.file.{Files, Paths}

class AxiReadArbTestDut(memoryFIle: String) extends Module {
  val io = IO(new Bundle {
    val in = Vec(2, new AXISlaveIO(32, 64))
  })

  val axi_r_arb = Module(new AxiReadArbiter)
  val axi_mem = Module(
    new AXI4Memory(
      AXI_AW = 32,
      AXI_DW = 64,
      INTERNAL_MEM_SIZE = 2048,
      INTERNAL_MEM_DW = 64,
      INTERNAL_MEM_BASE = 0,
      memoryFile = memoryFIle
    )
  )

  axi_mem.io <> axi_r_arb.io.out
  axi_r_arb.io.in <> io.in
}

class AxiReadArbTest extends AnyFreeSpec with ChiselScalatestTester {

  val memfile = "src/main/resources/random_file.bin"
  val byteArray = Files.readAllBytes(Paths.get(memfile))
  val mem: Seq[Byte] = byteArray.toSeq

  def addr_channel_gen(ADDR_WIDTH: Int, BURST_EN: Boolean) = {
    require(ADDR_WIDTH % 4 == 0, "ADDR_WIDTH must be a multiple of 4")

    val addr_gen = Gen.choose(0, 992)

    val len_gen = Gen.choose(0, 7)
    val id_gen = Gen.choose(0, 15)
    val size_gen = Gen.oneOf(SIZE_4, SIZE_8)
    val burst = BURST_INCR

    val size = size_gen.sample.get
    val real_size = if (size == SIZE_4) 4 else 8

    val addr_aligned = addr_gen.sample.get & ~(real_size - 1)

    val addr = new AXIAddressChannel(ADDR_WIDTH).Lit(
      _.id -> id_gen.sample.get.U,
      _.addr -> addr_aligned.U,
      _.len -> {
        if (BURST_EN) len_gen.sample.get.U else 0.U
      },
      _.size -> size,
      _.burst -> burst,
      _.lock -> 0.U,
      _.cache -> 0.U,
      _.prot -> 0.U,
      _.qos -> 0.U,
      _.region -> 0.U,
      _.user -> 0.U
    )
    addr
  }

  def gen_r_resp(ar: AXIAddressChannel) = {
    val aligned_addr = (ar.addr.litValue & ~(8 - 1)).toInt
    new AXIReadDataChannel(64).Lit(
      _.id -> ar.id,
      _.data -> long2UInt64(
        byteSeq2Uint64LittleEndian(
          mem.slice(aligned_addr, aligned_addr + 8)
        )
      ),
      _.resp -> 0.U,
      _.last -> true.B,
      _.user -> 0.U
    )
  }

  "AxiReadArbTest1" in {
    test(new AxiReadArbTestDut(memfile)).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
    ) { dut =>
      // -----------------------
      // init port
      // -----------------------
      dut.io.in.foreach(axi => {
        axi.ar.initSource().setSourceClock(dut.clock)
        axi.r.initSink().setSinkClock(dut.clock)
        axi.aw.initSource().setSourceClock(dut.clock)
        axi.w.initSource().setSourceClock(dut.clock)
        axi.b.initSink().setSinkClock(dut.clock)
      })

      dut.clock.step(5)

      // -----------------------
      // prepare test data
      // -----------------------

      val ar_seq1 = Seq.tabulate(1000) { i =>
        addr_channel_gen(32, false)
      }
      val ar_seq2 = Seq.tabulate(100) { i =>
        addr_channel_gen(32, false)
      }

      val r_seq1 = ar_seq1.map(ar => {
        gen_r_resp(ar)
      })
      val r_seq2 = ar_seq2.map(ar => {
        gen_r_resp(ar)
      })

      // -----------------------
      // test without bubble
      // -----------------------

      fork {
        dut.io.in(0).ar.enqueueSeq(ar_seq1)
      }.fork {
        dut.io.in(1).ar.enqueueSeq(ar_seq2)
      }.fork {
        dut.io.in(0).r.expectDequeueSeq(r_seq1)
      }.fork {
        dut.io.in(1).r.expectDequeueSeq(r_seq2)
      }.joinAndStep(dut.clock)

      dut.clock.step(5)
      // -----------------------
      // test with bubble
      // -----------------------

      fork {
        ar_seq1.foreach(ar => {
          dut.io.in(0).ar.enqueue(ar)
          dut.clock.step(Gen.choose(1, 10).sample.get)
        })
      }.fork {
        ar_seq2.foreach(ar => {
          dut.io.in(1).ar.enqueue(ar)
          dut.clock.step(Gen.choose(1, 10).sample.get)
        })
      }.fork {
        r_seq1.foreach(r => {
          dut.io.in(0).r.expectDequeue(r)
          dut.clock.step(Gen.choose(1, 10).sample.get)
        })
      }.fork {
        r_seq2.foreach(r => {
          dut.io.in(1).r.expectDequeue(r)
          dut.clock.step(Gen.choose(1, 10).sample.get)
        })
      }.joinAndStep(dut.clock)

    }
  }
}
