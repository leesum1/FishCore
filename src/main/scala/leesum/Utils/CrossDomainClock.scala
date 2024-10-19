package leesum.Utils

import chisel3._
import chisel3.util.{Decoupled, Queue}
import leesum.GenVerilogHelper

class CDC2PhaseSrc[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val sync_in = Flipped(Decoupled(gen))
    val async_out = Decoupled(gen)
  })

  // Registers with reset
  val req_src_q = RegInit(false.B)
  val ack_src_q = RegInit(false.B)
  val ack_q = RegInit(false.B)
  val data_src_q = RegInit(0.U.asTypeOf(gen))

  // The req_src and data_src registers change when a new data item is accepted.
  when(io.sync_in.fire) {
    req_src_q := ~req_src_q
    data_src_q := io.sync_in.bits
  }

  // The ack_src and ack registers act as synchronization stages.
  ack_src_q := io.async_out.ready
  ack_q := ack_src_q

  // Output assignments.
  io.sync_in.ready := (req_src_q === ack_q)
  io.async_out.valid := req_src_q
  io.async_out.bits := data_src_q
}

class CDC2PhaseDst[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val sync_out = Decoupled(gen)
    val async_in = Flipped(Decoupled(gen))
  })

  // Registers with reset
  val req_dst_q = RegInit(false.B)
  val req_q0 = RegInit(false.B)
  val req_q1 = RegInit(false.B)
  val ack_dst_q = RegInit(false.B)
  val data_dst_q = RegInit(0.U.asTypeOf(gen))

  // The ack_dst register changes when a new data item is accepted.
  when(io.sync_out.fire) {
    ack_dst_q := ~ack_dst_q
  }

  // The data_dst register changes when a new data item is presented.
  when(req_q0 =/= req_q1 && !io.sync_out.valid) {
    data_dst_q := io.async_in.bits
  }

  // The req_dst and req registers act as synchronization stages.
  req_dst_q := io.async_in.valid
  req_q0 := req_dst_q
  req_q1 := req_q0

  // Output assignments.
  io.sync_out.valid := (ack_dst_q =/= req_q1)
  io.sync_out.bits := data_dst_q
  io.async_in.ready := ack_dst_q
}

class CDCHandShake[T <: Data](data_type: T) extends RawModule {
  val io = IO(new Bundle {
    val clk_src = Input(Clock())
    val rst_src = Input(Bool()) // async reset on high
    val clk_dst = Input(Clock())
    val rst_dst = Input(Bool()) // async reset on high

    val src_hs = Flipped(Decoupled(data_type))
    val dst_hs = Decoupled(data_type)
  })

  val src_domain = withClockAndReset(io.clk_src, io.rst_src.asAsyncReset) {
    Module(new CDC2PhaseSrc(data_type))
  }
  val dst_domain = withClockAndReset(io.clk_dst, io.rst_dst.asAsyncReset) {
    Module(new CDC2PhaseDst(data_type))
  }

  // io.req_clkA <> Adomain_req_master <> Bdomain_req_slave <> io.req_clkB
  io.src_hs <> src_domain.io.sync_in
  src_domain.io.async_out <> dst_domain.io.async_in
  dst_domain.io.sync_out <> io.dst_hs
}

class CDCHandShakeReqResp[T <: Data, U <: Data](req_type: T, resp_type: U)
    extends RawModule {
  val io = IO(new Bundle {

    val clk_src = Input(Clock())
    val rst_src = Input(Bool()) // async reset on high
    val clk_dst = Input(Clock())
    val rst_dst = Input(Bool()) // async reset on high

    val req_src = Flipped(Decoupled(req_type))
    val resp_src = Decoupled(resp_type)

    val req_dst = Decoupled(req_type)
    val resp_dst = Flipped(Decoupled(resp_type))
  })

  val req_cdc_hs = Module(new CDCHandShake(req_type))

  req_cdc_hs.io.clk_src := io.clk_src
  req_cdc_hs.io.rst_src := io.rst_src
  req_cdc_hs.io.clk_dst := io.clk_dst
  req_cdc_hs.io.rst_dst := io.rst_dst

  io.req_src <> req_cdc_hs.io.src_hs
  req_cdc_hs.io.dst_hs <> io.req_dst

  val resp_cdc_hs = Module(new CDCHandShake(resp_type))
  resp_cdc_hs.io.clk_src := io.clk_dst
  resp_cdc_hs.io.rst_src := io.rst_dst
  resp_cdc_hs.io.clk_dst := io.clk_src
  resp_cdc_hs.io.rst_dst := io.rst_src

  io.resp_dst <> resp_cdc_hs.io.src_hs
  resp_cdc_hs.io.dst_hs <> io.resp_src

}

object CDCHandShakeReqRespGenVerilog extends App {
  GenVerilogHelper(new CDCHandShakeReqResp(UInt(32.W), UInt(32.W)))
}

object CDCHandShakepGenVerilog extends App {
  GenVerilogHelper(new CDCHandShake(UInt(32.W)))
}
