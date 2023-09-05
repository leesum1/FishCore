package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import leesum.axi4.AXIDef.{BURST_INCR, SIZE_4, SIZE_8}
import leesum.axi4.{
  AXI4Memory,
  AXIAddressChannel,
  AXIReadDataChannel,
  AXIWriteDataChannel,
  AXIWriteResponseChannel
}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

// TODO: test list
//  1. back to back read
//  2. loopback test
//  3. read write the same address
//  4. narrow transfer
//  5. unaligned transfer
class AXI4MemoryTest extends AnyFreeSpec with ChiselScalatestTester {

  def addr_channel_gen(ADDR_WIDTH: Int, BURST_EN: Boolean) = {
    require(ADDR_WIDTH % 4 == 0, "ADDR_WIDTH must be a multiple of 4")

    val addr_gen = Gen
      .listOfN(ADDR_WIDTH / 4, Gen.hexChar)
      .map("x" + _.mkString)
      .map(_.U(ADDR_WIDTH.W))

    val len_gen = Gen.choose(0, 7)
    val id_gen = Gen.choose(0, 15)
    val size_gen = Gen.oneOf(SIZE_4, SIZE_8)
    val burst = BURST_INCR

    val addr = new AXIAddressChannel(ADDR_WIDTH).Lit(
      _.id -> id_gen.sample.get.U,
      _.addr -> addr_gen.sample.get,
      _.len -> { if (BURST_EN) len_gen.sample.get.U else 0.U },
      _.size -> size_gen.sample.get,
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
  def w_channel_gen(DATA_WIDTH: Int, LEN: Int): Seq[AXIWriteDataChannel] = {
    val data_gen = Gen.choose(0L, 0xffffffffL).map(_.U(DATA_WIDTH.W))
    val strb_gen =
      Gen.choose(0, (1 << (DATA_WIDTH / 8)) - 1).map(_.U((DATA_WIDTH / 8).W))

    val data_seq = Seq.fill(LEN + 1)(data_gen.sample.get)
    val strb_seq = Seq.fill(LEN + 1)(strb_gen.sample.get)
    val last_seq = Seq.fill(LEN)(false.B) ++ Seq(true.B)

    val w_channel_seq = data_seq
      .zip(strb_seq)
      .zip(last_seq)
      .map { case ((data, strb), last) =>
        (new AXIWriteDataChannel(DATA_WIDTH).Lit(
          _.data -> data,
          _.strb -> strb,
          _.last -> last,
          _.user -> 0.U
        ))
      }
    w_channel_seq
  }

  def b_channel_gen(ID: Int) = {
    (new AXIWriteResponseChannel().Lit(
      _.id -> ID.U,
      _.resp -> 0.U,
      _.user -> 0.U
    ))
  }

  "narrow_transfer_test" in {
    test(new AXI4Memory())
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.ar.initSource()
        dut.io.ar.setSourceClock(dut.clock)
        dut.io.r.initSink()
        dut.io.r.setSinkClock(dut.clock)
        dut.io.aw.initSource()
        dut.io.aw.setSourceClock(dut.clock)
        dut.io.w.initSource()
        dut.io.w.setSourceClock(dut.clock)
        dut.io.b.initSink()
        dut.io.b.setSinkClock(dut.clock)

        val w_addr = new AXIAddressChannel(32).Lit(
          _.id -> 2.U,
          _.addr -> 4.U,
          _.len -> 0.U,
          _.size -> SIZE_4,
          _.burst -> BURST_INCR,
          _.lock -> 0.U,
          _.cache -> 0.U,
          _.prot -> 0.U,
          _.qos -> 0.U,
          _.region -> 0.U,
          _.user -> 0.U
        )

        val w_data = new AXIWriteDataChannel(64).Lit(
          _.data -> "x1122334455667788".U,
          _.strb -> 0x0f.U,
          _.last -> true.B,
          _.user -> 0.U
        )

        val r_data = new AXIReadDataChannel(64).Lit(
          _.data -> "x0000000055667788".U,
          _.resp -> 0.U,
          _.last -> true.B,
          _.user -> 0.U,
          _.id -> w_addr.id
        )

        // prepare write data
        dut.io.aw.enqueue(w_addr)
        dut.io.w.enqueue(w_data)
        dut.io.b.expectDequeue(b_channel_gen(w_addr.id.litValue.toInt))

        // read data
        dut.io.ar.enqueue(w_addr)
        dut.io.r.expectDequeue(r_data)

      }
  }

  // basic loopback test
  // 1. address aligned, burst transfer
  "loopback_test" in {
    test(new AXI4Memory())
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.ar.initSource()
        dut.io.ar.setSourceClock(dut.clock)
        dut.io.r.initSink()
        dut.io.r.setSinkClock(dut.clock)
        dut.io.aw.initSource()
        dut.io.aw.setSourceClock(dut.clock)
        dut.io.w.initSource()
        dut.io.w.setSourceClock(dut.clock)
        dut.io.b.initSink()
        dut.io.b.setSinkClock(dut.clock)

        val aw_addr = new AXIAddressChannel(32).Lit(
          _.id -> 2.U,
          _.addr -> 8.U,
          _.len -> 31.U,
          _.size -> SIZE_8,
          _.burst -> BURST_INCR,
          _.lock -> 0.U,
          _.cache -> 0.U,
          _.prot -> 0.U,
          _.qos -> 0.U,
          _.region -> 0.U,
          _.user -> 0.U
        )

        val w_seq = 0
          .to(aw_addr.len.litValue.toInt)
          .map(idx =>
            new AXIWriteDataChannel(64).Lit(
              _.data -> (idx * 1024).U,
              _.strb -> 0xff.U,
              _.last -> (idx == (aw_addr.len.litValue.toInt)).B, // Set last to true for the last element
              _.user -> 0.U
            )
          )
        assert(w_seq.last.last.litValue == 1)

        val r_seq = w_seq.map(w => {
          new AXIReadDataChannel(64).Lit(
            _.data -> w.data,
            _.resp -> 0.U,
            _.last -> w.last,
            _.user -> 0.U,
            _.id -> aw_addr.id
          )
        })

        dut.clock.step(5)

        /** start to test */
        // write data
        dut.io.aw.enqueue(aw_addr)
        dut.io.w.enqueueSeq(w_seq)
        dut.io.b.expectDequeue(b_channel_gen(aw_addr.id.litValue.toInt))
        // read data
        dut.io.ar.enqueue(aw_addr)
        dut.io.r.expectDequeueSeq(r_seq)
//        dut.io.r.ready.poke(true.B)

        dut.clock.step(100)
      }
  }

  "back_to_back" in {
    test(new AXI4Memory())
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.ar.initSource()
        dut.io.ar.setSourceClock(dut.clock)
        dut.io.r.initSink()
        dut.io.r.setSinkClock(dut.clock)
        dut.io.aw.initSource()
        dut.io.aw.setSourceClock(dut.clock)
        dut.io.w.initSource()
        dut.io.w.setSourceClock(dut.clock)
        dut.io.b.initSink()
        dut.io.b.setSinkClock(dut.clock)

        dut.clock.step(5)

        // read test data
        val ar_burst_seq =
          0.until(10).map(_ => addr_channel_gen(32, BURST_EN = true))
        val ar_single_seq =
          0.until(10).map(_ => addr_channel_gen(32, BURST_EN = false))
        // write test data
        val aw_burst_seq =
          0.until(10).map(_ => addr_channel_gen(32, BURST_EN = true))
        val w_burst_seq = aw_burst_seq.map(aw => {
          w_channel_gen(64, aw.len.litValue.toInt)
        })
        val b_burst_seq = aw_burst_seq.map(aw => {
          b_channel_gen(aw.id.litValue.toInt)
        })

        val aw_signal_seq =
          0.until(10).map(_ => addr_channel_gen(32, BURST_EN = false))
        val w_signal_seq = aw_signal_seq.map(aw => {
          w_channel_gen(64, aw.len.litValue.toInt)
        })
        val b_signal_seq = aw_signal_seq.map(aw => {
          b_channel_gen(aw.id.litValue.toInt)
        })

        // read back to back without bubble
        // always ready to accept read response
        dut.io.r.ready.poke(true)
        dut.io.ar.enqueueSeq(ar_burst_seq)
        dut.io.ar.enqueueSeq(ar_single_seq)

        // write back to back without bubble
        // aw, w, b channel are independent
        fork {
          // aw channel
          dut.io.aw.enqueueSeq(aw_burst_seq)
          dut.io.aw.enqueueSeq(aw_signal_seq)
        }.fork {
          // w channel
          w_burst_seq.foreach(w => {
            dut.io.w.enqueueSeq(w)
          })
          w_signal_seq.foreach(w => {
            dut.io.w.enqueueSeq(w)
          })
        }.fork {
          // b channel
          b_burst_seq.foreach(b => {
            dut.io.b.expectDequeue(b)
          })
          b_signal_seq.foreach(b => {
            dut.io.b.expectDequeue(b)
          })
        }.join()
      }
  }
}
