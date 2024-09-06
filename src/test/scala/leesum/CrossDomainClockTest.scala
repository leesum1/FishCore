package leesum

import chisel3._
import chisel3.util.Decoupled
import chiseltest._
import leesum.Utils.{CDCHandShakeReqResp, ClockGenerator}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class CDCHandShakeTestWrapper[T <: Data, U <: Data](
    req_type: T,
    resp_type: U,
    multi_clk_en: Boolean, // true: multi clock domain, false: one clock domain
    slow_to_fast: Boolean =
      false // true: slow clock to fast clock, false: fast clock to slow clock
) extends Module {
  val io = IO(new Bundle {
    val req_clkA = Flipped(Decoupled(req_type))
    val resp_clkA = Decoupled(resp_type)

    val req_clkB = Decoupled(req_type)
    val resp_clkB = Flipped(Decoupled(resp_type))
  })

  if (multi_clk_en) {
    val clk_config = if (slow_to_fast) {
      Seq(("clkA", 20, 3), ("clkB", 6, 2))
    } else {
      Seq(("clkA", 6, 1), ("clkB", 10, 1))
    }

    val div_clocks = Module(
      new ClockGenerator(clk_config)
    )
    val my_reset = reset.asBool

    withClockAndReset(div_clocks.io.clocks(0).asClock, my_reset) {
      val cdc_hs = Module(new CDCHandShakeReqResp(req_type, resp_type))
      cdc_hs.io.req_clkA <> io.req_clkA
      cdc_hs.io.resp_clkA <> io.resp_clkA
      io.req_clkB <> cdc_hs.io.req_clkB
      io.resp_clkB <> cdc_hs.io.resp_clkB
      cdc_hs.io.clkB := div_clocks.io.clocks(1).asClock
      cdc_hs.io.rstB := my_reset
    }
  } else {
    val cdc_hs = Module(new CDCHandShakeReqResp(req_type, resp_type))
    cdc_hs.io.req_clkA <> io.req_clkA
    cdc_hs.io.resp_clkA <> io.resp_clkA
    io.req_clkB <> cdc_hs.io.req_clkB
    io.resp_clkB <> cdc_hs.io.resp_clkB
    cdc_hs.io.clkB := clock
    cdc_hs.io.rstB := reset.asBool
  }
}

class CrossDomainClockTest extends AnyFreeSpec with ChiselScalatestTester {

  "CDCClockTest" in {
    test(
      new CDCHandShakeTestWrapper(
        UInt(32.W),
        UInt(32.W),
        multi_clk_en = true
      )
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.clock.setTimeout(100000)

        dut.reset.poke(true.B)
        dut.clock.step(200)
        dut.reset.poke(false.B)
        dut.clock.step(2000)
      }
  }

  "CDCHandShakeReqRespOneClockTest" in {
    test(
      new CDCHandShakeTestWrapper(UInt(32.W), UInt(32.W), multi_clk_en = false)
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        val req_data_seq = Seq.tabulate(2000)(i => i.U)
        val resp_data_seq = Seq.tabulate(2000)(i => (i + 2000).U)

        crate_req_resp_testcase(dut, req_data_seq, resp_data_seq)
        dut.clock.step(5)
        crate_req_resp_testcase(
          dut,
          req_data_seq,
          resp_data_seq,
          delay_en = true
        )
        dut.clock.step(5)
      }
  }

  "CDCHandShakeReqRespMultiClockS2FTest" in {
    test(
      new CDCHandShakeTestWrapper(
        UInt(32.W),
        UInt(32.W),
        multi_clk_en = true,
        slow_to_fast = true
      )
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        val req_data_seq = Seq.tabulate(1000)(i => i.U)
        val resp_data_seq = Seq.tabulate(1000)(i => (i + 2000).U)

        dut.clock.step(100)
        crate_req_resp_testcase(dut, req_data_seq, resp_data_seq)
        dut.clock.step(5)
        crate_req_resp_testcase(
          dut,
          req_data_seq,
          resp_data_seq,
          delay_en = true
        )
        dut.clock.step(5)
      }
  }

  "CDCHandShakeReqRespMultiClockF2STest" in {
    test(
      new CDCHandShakeTestWrapper(
        UInt(32.W),
        UInt(32.W),
        multi_clk_en = true,
        slow_to_fast = false
      )
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        val req_data_seq = Seq.tabulate(1000)(i => i.U)
        val resp_data_seq = Seq.tabulate(1000)(i => (i + 2000).U)

        dut.clock.step(100)
        crate_req_resp_testcase(dut, req_data_seq, resp_data_seq)
        dut.clock.step(5)
        crate_req_resp_testcase(
          dut,
          req_data_seq,
          resp_data_seq,
          delay_en = true
        )
        dut.clock.step(5)
      }
  }

  private def crate_req_resp_testcase[T <: Data, U <: Data](
      dut: CDCHandShakeTestWrapper[T, U],
      req_data_seq: Seq[T],
      resp_data_seq: Seq[U],
      delay_en: Boolean = false
  ): Unit = {
    val rand_delay = (math.random() * 50 + 10).toInt

    fork {
      for ((req_data, resp_data) <- req_data_seq.zip(resp_data_seq)) {
        println(s"req_data: ${req_data}, resp_data: ${resp_data}")
        if (delay_en) {
          dut.clock.step(rand_delay)
        }
        dut.io.req_clkA.enqueue(req_data)
        if (delay_en) {
          dut.clock.step(rand_delay)
        }
        dut.io.resp_clkA.expectDequeue(resp_data)
      }
    }.fork {
      for ((req_data, resp_data) <- req_data_seq.zip(resp_data_seq)) {
        if (delay_en) {
          dut.clock.step(rand_delay)
        }
        dut.io.req_clkB.expectDequeue(req_data)
        if (delay_en) {
          dut.clock.step(rand_delay)
        }
        dut.io.resp_clkB.enqueue(resp_data)
      }
    }.joinAndStep()
  }
}
