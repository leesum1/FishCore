package leesum.Utils

import chisel3._
import chisel3.util.{ShiftRegister, log2Ceil}
import leesum.GenVerilogHelper

class ClockDiv(val period: Int, val shift_cnt: Int = 0) extends Module {
  val io = IO(new Bundle {
    val clk_div = Output(Bool())
  })

  val half_period = period / 2

  require(half_period > 1, "half_period must be greater than 1")
  require(
    shift_cnt >= 0 && shift_cnt < half_period,
    "shift_cnt must be greater than or equal to 0"
  )

  val cnt = RegInit(0.U(log2Ceil(half_period).W))
  val period_clk = RegInit(false.B)
  when(cnt === (half_period - 1).U) {
    cnt := 0.U
    period_clk := ~period_clk
  }.otherwise {
    cnt := cnt + 1.U
  }

  io.clk_div := ShiftRegister(period_clk, shift_cnt)
}

object ClockDiv extends App {
  def apply(period: Int, shift_cnt: Int = 0) = {
    val out_clk =
      Module(new ClockDiv(period, shift_cnt))
        .suggestName(s"clk_div_${period}p_${shift_cnt}off")
    out_clk.io.clk_div
  }
}

class ClockGenerator(
    clock_config: Seq[(String, Int, Int)] // (name, period, shift_cnt)
) extends Module {

  val io = IO(new Bundle {
    val clocks = Output(Vec(clock_config.size, Bool()))
  })

  clock_config.zipWithIndex.foreach { case ((name, period, shift_cnt), i) =>
    io.clocks(i) := ClockDiv(period, shift_cnt)
  }
}

object ClockGenerator extends App {
  val clock_config = Seq(
    ("clk_2", 2, 0),
    ("clk_4", 4, 0),
    ("clk_8", 8, 0),
    ("clk_16", 16, 0),
    ("clk_32", 32, 0),
    ("clk_64", 64, 10),
    ("clk_128", 128, 0)
  )
  GenVerilogHelper(new ClockGenerator(clock_config))
}
