package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import leesum.axi4.AXIDef.{BURST_INCR, SIZE_4, SIZE_8}
import leesum.axi4.{
  AXI4Memory,
  AXIAddressChannel,
  AXIWriteDataChannel,
  AXIWriteResponseChannel
}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

class AXI4MemoryTest extends AnyFreeSpec with ChiselScalatestTester {

  def addr_channel_gen(ADDR_WIDTH: Int): Unit = {
    val addr_gen = Gen.choose(0, (1 << ADDR_WIDTH) - 1)
    val len_gen = Gen.choose(0, 7)
    val size_gen = Gen.oneOf(SIZE_4, SIZE_8)
    val burst = BURST_INCR
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

  "axi_mem_test" in {
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

        val ar_burst8 = (new AXIAddressChannel(32)).Lit(
          _.id -> 0.U,
          _.addr -> 4.U,
          _.len -> 7.U,
          _.size -> SIZE_4,
          _.burst -> BURST_INCR,
          _.lock -> 0.U,
          _.cache -> 0.U,
          _.prot -> 0.U,
          _.qos -> 0.U,
          _.region -> 0.U,
          _.user -> 0.U
        )
        val ar_signal = (new AXIAddressChannel(32)).Lit(
          _.id -> 0.U,
          _.addr -> 8.U,
          _.len -> 0.U,
          _.size -> SIZE_8,
          _.burst -> BURST_INCR,
          _.lock -> 0.U,
          _.cache -> 0.U,
          _.prot -> 0.U,
          _.qos -> 0.U,
          _.region -> 0.U,
          _.user -> 0.U
        )
        val aw_burst8 = (new AXIAddressChannel(32)).Lit(
          _.id -> 2.U,
          _.addr -> 4.U,
          _.len -> 7.U,
          _.size -> SIZE_4,
          _.burst -> BURST_INCR,
          _.lock -> 0.U,
          _.cache -> 0.U,
          _.prot -> 0.U,
          _.qos -> 0.U,
          _.region -> 0.U,
          _.user -> 0.U
        )
        val aw_signal = (new AXIAddressChannel(32)).Lit(
          _.id -> 2.U,
          _.addr -> 16.U,
          _.len -> 0.U,
          _.size -> SIZE_8,
          _.burst -> BURST_INCR,
          _.lock -> 0.U,
          _.cache -> 0.U,
          _.prot -> 0.U,
          _.qos -> 0.U,
          _.region -> 0.U,
          _.user -> 0.U
        )

        dut.clock.step(5)
        fork
          .withRegion(Monitor) {
            // r
            dut.io.ar.enqueueSeq(Seq.fill(10)(ar_burst8))
            dut.clock.step(10)
            dut.io.ar.enqueueSeq(Seq.fill(5)(ar_signal))
//            dut.clock.step(10)
            dut.io.ar.enqueueSeq(Seq.fill(5)(ar_burst8))
            // w
            dut.io.aw.enqueue(aw_burst8)

            fork
              .withRegion(Monitor) {
                dut.io.w.enqueueSeq(w_channel_gen(64, 7))
              }
              .fork {
                dut.io.aw.enqueue(aw_burst8)
              }
              .joinAndStep(dut.clock)
            dut.io.b.expectDequeue(b_channel_gen(2))
//            // w
//            dut.clock.step(1)
//            dut.io.aw.enqueue(aw_burst8)
//            dut.io.w.enqueueSeq(w_channel_gen(64, 7))
//            dut.io.b.expectDequeue(b_channel_gen(2))

          }
          .fork {
            var count = 2000
            while (count > 0) {
              count = count - 1
              dut.io.r.ready.poke(Random.nextBoolean())
              dut.clock.step(1)
            }
          }
          .joinAndStep(dut.clock)
      }
  }
}
