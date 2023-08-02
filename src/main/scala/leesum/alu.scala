package leesum

import Chisel.{Cat, Fill, Mux1H}
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util.{Decoupled, MuxLookup, PopCount, Reverse, is, switch}

class AluIn extends Bundle {
  private val alu_width = 64
  val a = Input(UInt(alu_width.W))
  val b = Input(UInt(alu_width.W))
  val op = Input(AluOP())
  val rvw = Input(OPWidth())
}
class AluOut extends Bundle {
  private val alu_width = 64
  val res = Output(UInt(alu_width.W))
}

class FuAlu extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AluIn))
    val out = Decoupled(new AluOut)
  })

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid

  val add_res = io.in.bits.a + io.in.bits.b
  val sub_res = io.in.bits.a - io.in.bits.b
  val and_res = io.in.bits.a & io.in.bits.b
  val or_res = io.in.bits.a | io.in.bits.b
  val xor_res = io.in.bits.a ^ io.in.bits.b

  val slt_res = io.in.bits.a.asSInt < io.in.bits.b.asSInt
  val sltu_res = io.in.bits.a < io.in.bits.b

  /* shift module Start*/
  val shift = Module(new AluShift())

  shift.io.shift_in := io.in.bits.a
  shift.io.shift_count := io.in.bits.b
  shift.io.sra_req := io.in.bits.op === AluOP.Sra
  shift.io.srl_req := io.in.bits.op === AluOP.Srl
  shift.io.sll_req := io.in.bits.op === AluOP.Sll
  shift.io.shift32_req := io.in.bits.rvw === OPWidth.W32
  val shift_res = shift.io.shift_out
  /* shift module End*/

  val resonehot = Mux1H(
    Seq(
      (io.in.bits.op === AluOP.Add) -> add_res,
      (io.in.bits.op === AluOP.Sub) -> sub_res,
      (io.in.bits.op === AluOP.And) -> and_res,
      (io.in.bits.op === AluOP.Or) -> or_res,
      (io.in.bits.op === AluOP.Xor) -> xor_res,
      (io.in.bits.op === AluOP.Sll) -> shift_res,
      (io.in.bits.op === AluOP.Srl) -> shift_res,
      (io.in.bits.op === AluOP.Sra) -> shift_res,
      (io.in.bits.op === AluOP.Slt) -> slt_res,
      (io.in.bits.op === AluOP.Sltu) -> sltu_res
    )
  )

  io.out.bits.res := Mux(
    io.in.bits.rvw === OPWidth.W32,
    RiscvTools.sign_ext(resonehot(31, 0), 32, 64),
    resonehot
  )
}

object gen_alu_verilog extends App {
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new FuAlu(),
      Array("--target-dir", verilogDir)
    )
}

class AluShift extends Module {
  val io = IO(new Bundle {
    val sra_req = Input(Bool())
    val srl_req = Input(Bool())
    val sll_req = Input(Bool())
    val shift32_req = Input(Bool())
    val shift_in = Input(UInt(64.W))
    val shift_count = Input(UInt(6.W))
    val shift_out = Output(UInt(64.W))
  })
  // Ensure that at most one of the shift control signals is active
  assert(
    PopCount(Seq(io.sra_req, io.srl_req, io.sll_req)) <= 1.U,
    "At most one shift control signal should be active"
  )
  val shift_req = io.sra_req || io.srl_req || io.sll_req

  val shift_op1_tmp = {
    val shift_op1_hi32 =
      Mux(
        io.shift32_req,
        Fill(32, io.shift_in(31) & io.sra_req),
        io.shift_in(63, 32)
      )
    Cat(
      shift_op1_hi32,
      io.shift_in(31, 0)
    )
  }
  val shift_op1 = Mux(io.sll_req, Reverse(shift_op1_tmp), shift_op1_tmp)
  val shift_op2 = Mux(io.shift32_req, io.shift_count(4, 0), io.shift_count)
  val shout_r = (Cat(
    io.sra_req & shift_op1(63),
    shift_op1
  ).asSInt >> shift_op2)(63, 0)
  val shout_l = Reverse(shout_r)

  val shout = Mux1H(
    Seq(
      io.sra_req -> shout_r,
      io.srl_req -> shout_r,
      io.sll_req -> shout_l
    )
  )

  io.shift_out := Mux(io.shift32_req, Cat(0.U(32), shout(31, 0)), shout)

}
object gen_shift_verilog extends App {
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new AluShift(),
      Array("--target-dir", verilogDir)
    )
}
