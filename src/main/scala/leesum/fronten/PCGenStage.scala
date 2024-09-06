package leesum.fronten

import chisel3._
import chisel3.util.{Cat, Decoupled}
import leesum.{CheckAligned, GenVerilogHelper, RedirectPC}

class PCGenStage(boot_pc: Long, rvc_en: Boolean = false) extends Module {

  require(boot_pc % 8 == 0, "boot_pc must be aligned to 8")

  val fetch_size = 8

  val io = IO(new Bundle {
    val commit_redirect_pc = Input(new RedirectPC)
    val pc = Decoupled(UInt(64.W))
    val npc = Output(UInt(64.W))
    val f1_redirect_pc = Input(new RedirectPC)
    val f3_redirect_pc = Input(new RedirectPC)
  })
  val pc_reg = RegInit((boot_pc - fetch_size).U(64.W))

  val npc = Wire(UInt(64.W))
  val npc_buf = RegInit(0.U(64.W))
  val npc_buf_valid = RegInit(false.B)

  when(io.commit_redirect_pc.valid) {
    npc := io.commit_redirect_pc.target
    npc_buf := io.commit_redirect_pc.target
    npc_buf_valid := true.B
  }.elsewhen(io.f3_redirect_pc.valid) {
    npc := io.f3_redirect_pc.target
    npc_buf := io.f3_redirect_pc.target
    npc_buf_valid := true.B
  }.elsewhen(io.f1_redirect_pc.valid) {
    npc := io.f1_redirect_pc.target
  }.otherwise {
    npc := Cat(pc_reg(63, 3), "b000".U(3.W)) + fetch_size.U
  }

  when(io.pc.fire) {
    npc_buf_valid := false.B
    pc_reg := Mux(npc_buf_valid, npc_buf, npc)
  }

  io.pc.valid := !reset.asBool
  io.pc.bits := pc_reg
  io.npc := Mux(npc_buf_valid, npc_buf, npc)

  // ---------------------
  // assert
  // ---------------------

//  when(io.f1_redirect_pc.valid) {
//    assert(io.pc.fire, "pc must be valid when f1_redirect_pc is valid")
//  }

  assert(
    CheckAligned(pc_reg, if (rvc_en) 1.U(2.W) else 2.U(2.W)),
    "pc_reg must be aligned to %d".format(if (rvc_en) 2 else 4)
  )

}

object gen_pc_gen_stage_verilog extends App {
  GenVerilogHelper(new PCGenStage(1000))
}
