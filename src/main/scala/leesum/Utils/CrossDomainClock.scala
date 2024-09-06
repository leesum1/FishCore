package leesum.Utils

import chisel3._
import chisel3.util.{Decoupled, Enum, ShiftRegister, is, switch}
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

class CDCHandShakeReqResp[T <: Data, U <: Data](req_type: T, resp_type: U)
    extends Module {
  val io = IO(new Bundle {

    val clkB = Input(Clock())
    val rstB = Input(Bool())

    val req_clkA = Flipped(Decoupled(req_type))
    val resp_clkA = Decoupled(resp_type)

    val req_clkB = Decoupled(req_type)
    val resp_clkB = Flipped(Decoupled(resp_type))
  })

  val Adomain_req_master = Module(new CDC2PhaseSrc(req_type))
  val Adomain_resp_slave = Module(new CDC2PhaseDst(resp_type))

  val Bdomain_req_slave = withClockAndReset(io.clkB, io.rstB) {
    Module(new CDC2PhaseDst(req_type))
  }
  val Bdomain_resp_master = withClockAndReset(io.clkB, io.rstB) {
    Module(new CDC2PhaseSrc(resp_type))
  }

  Adomain_req_master.io.sync_in <> io.req_clkA

  Adomain_req_master.io.async_out <> Bdomain_req_slave.io.async_in
  Bdomain_req_slave.io.sync_out <> io.req_clkB

  Bdomain_resp_master.io.sync_in <> io.resp_clkB
  Bdomain_resp_master.io.async_out <> Adomain_resp_slave.io.async_in
  Adomain_resp_slave.io.sync_out <> io.resp_clkA
}

object CDCHandShakeReqRespGenVerilog extends App {
  GenVerilogHelper(new CDCHandShakeReqResp(UInt(8.W), UInt(8.W)))
}
