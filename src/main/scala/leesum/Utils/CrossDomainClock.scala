package leesum.Utils

import chisel3._
import chisel3.util.{Decoupled, Enum, ShiftRegister, ShiftRegisters}
import leesum.GenVerilogHelper

class CDCHandShakeMaster[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val req_in = Flipped(Decoupled(gen))
    val req_out = Decoupled(gen)
  })

  // sync req_out_ready to the same domain as req_in
  val req_out_ready_stable =
    ShiftRegister(io.req_out.ready, 2, false.B, true.B)

  val req_out_valid = RegNext(io.req_in.valid, init = false.B)
//  val req_out_data = RegInit(0.U.asTypeOf(gen))

  io.req_out.valid := req_out_valid
  io.req_out.bits := io.req_in.bits
  io.req_in.ready := req_out_ready_stable
}

class CDCHandShakeSlave[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val req_in = Flipped(Decoupled(gen))
    val req_out = Decoupled(gen)
  })
  val req_in_valid_stable = ShiftRegister(io.req_in.valid, 2, false.B, true.B)
  val req_in_ready = RegNext(io.req_out.ready, init = false.B)

  io.req_out.valid := req_in_valid_stable
  io.req_in.ready := req_in_ready
  io.req_out.bits := io.req_in.bits
}

/** Cross domain handshake request response. A domain: Source domain ; B domain:
  * Destination domain
  */
class CDCHandShakeReqResp[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {

    val clkB = Input(Clock())
    val rstB = Input(Bool())

    val req_clkA = Flipped(Decoupled(gen))
    val resp_clkA = Decoupled(gen)

    val req_clkB = Decoupled(gen)
    val resp_clkB = Flipped(Decoupled(gen))
  })

  val Adomain_req_master = Module(new CDCHandShakeMaster(gen))
  val Adomain_resp_slave = Module(new CDCHandShakeSlave(gen))

  val Bdomain_req_slave = withClockAndReset(io.clkB, io.rstB) {
    Module(new CDCHandShakeSlave(gen))
  }
  val Bdomain_resp_master = withClockAndReset(io.clkB, io.rstB) {
    Module(new CDCHandShakeMaster(gen))
  }

  Adomain_req_master.io.req_in <> io.req_clkA
  Adomain_req_master.io.req_out <> Bdomain_req_slave.io.req_in
  Bdomain_req_slave.io.req_out <> io.req_clkB

  Bdomain_resp_master.io.req_in <> io.resp_clkB
  Bdomain_resp_master.io.req_out <> Adomain_resp_slave.io.req_in
  Adomain_resp_slave.io.req_out <> io.resp_clkA
}

object CDCHandShakeReqRespGenVeriog extends App {
  GenVerilogHelper(new CDCHandShakeReqResp(UInt(32.W)))
}

object CDCHandShakeMasterGenVeriog extends App {
  GenVerilogHelper(new CDCHandShakeMaster(UInt(32.W)))
}

object CDCHandShakeSlaveGenVeriog extends App {
  GenVerilogHelper(new CDCHandShakeSlave(UInt(32.W)))
}
