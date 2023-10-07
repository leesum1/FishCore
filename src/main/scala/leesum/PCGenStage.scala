package leesum
import chisel3._
import chisel3.util.{Cat, Decoupled}

class PCGenStage(boot_pc: Long) extends Module {

  require(boot_pc % 8 == 0, "boot_pc must be aligned to 8")

  val support_rvc = false
  val fetch_size = 8

  val io = IO(new Bundle {
    val redirect_pc = Flipped(Decoupled(new RedirectPC))
    val pc = Decoupled(UInt(64.W))
  })
  val pc_reg = RegInit((boot_pc).U(64.W))

  val npc = Wire(UInt(64.W))

  when(io.redirect_pc.valid) {
    npc := io.redirect_pc.bits.target
  }.otherwise {
    npc := Cat(pc_reg(63, 3), "b000".U(3.W)) + fetch_size.U
  }

  when(io.pc.fire) {
    pc_reg := npc
  }

  io.redirect_pc.ready := io.pc.fire

  io.pc.valid := !reset.asBool
  io.pc.bits := pc_reg

  // ---------------------
  // assert
  // ---------------------

  assert(
    CheckAligned(pc_reg, if (support_rvc) 1.U else 2.U),
    "pc_reg must be aligned to %d".format(if (support_rvc) 2 else 4)
  )

}

object gen_pc_gen_stage_verilog extends App {
  GenVerilogHelper(new PCGenStage(1000))
}
