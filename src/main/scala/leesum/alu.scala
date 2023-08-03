package leesum

import Chisel.{Cat, Fill, Mux1H}
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util.{Decoupled, PopCount, Reverse}

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
  /* Adder module Start */
  val adder = Module(new AluAdder())
  adder.io.sub_req := io.in.bits.op === AluOP.Sub || io.in.bits.op === AluOP.Slt || io.in.bits.op === AluOP.Sltu
  adder.io.adder_in1 := io.in.bits.a
  adder.io.adder_in2 := io.in.bits.b
  val adder_res = adder.io.adder_out
  val slt_res = adder.io.slt
  val sltu_res = adder.io.sltu
  /* Adder module End */

  /* Logic module Start */
  val and_res = io.in.bits.a & io.in.bits.b
  val or_res = io.in.bits.a | io.in.bits.b
  val xor_res = io.in.bits.a ^ io.in.bits.b
  /* Logic module End */

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
      (io.in.bits.op === AluOP.Add) -> adder_res,
      (io.in.bits.op === AluOP.Sub) -> adder_res,
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

/** This module is used to calculate the result of add,sub,slt,sltu. The
  * operation is done by using two's complement number. when sub_req is true,
  * the adder will do subtraction adder_out = a + (-b) = a + (~b) + 1, orthwise
  * do addition normally.The slt (signed)(a < b) and sltu (unsigned)(a < b)
  * signals were only valid when sub_req is true
  */
class AluAdder extends Module {
  val io = IO(new Bundle {
    val sub_req = Input(Bool())
    val adder_in1 = Input(UInt(64.W))
    val adder_in2 = Input(UInt(64.W))
    val adder_out = Output(UInt(64.W))
    val slt = Output(Bool())
    val sltu = Output(Bool())
    val eq = Output(Bool())
  })
  val adder_in2_inv =
    Mux(io.sub_req, (~io.adder_in2).asUInt + 1.U, io.adder_in2)

  // use two's complement to do subtraction and addition
  // https://en.wikipedia.org/wiki/Two%27s_complement
  // and extend the sign bit to 65 bits to detect overflow
  // https://blog.csdn.net/mariodf/article/details/125334271/

  val add_res =
    Cat(io.adder_in1(63), io.adder_in1) +& Cat(adder_in2_inv(63), adder_in2_inv)
  val flag_cf = add_res(65) ^ io.sub_req
  // https://electronics.stackexchange.com/questions/476250/signed-overflow-detection
  val flag_of = add_res(63) ^ add_res(64)

  val flag_sf = add_res(63)
  val flag_zf = add_res === 0.U

  val slt = flag_sf ^ flag_of
  val sltu = flag_cf

  io.adder_out := add_res
  io.slt := slt
  io.sltu := sltu
  io.eq := flag_zf
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
  // convert left shift to right shift logic
  val shift_op1 = Mux(io.sll_req, Reverse(shift_op1_tmp), shift_op1_tmp)
  val shift_op2 = Mux(io.shift32_req, io.shift_count(4, 0), io.shift_count)

  // right shift logic,extend the sign bit if sra_req is true
  val sra_srl_res = (Cat(
    io.sra_req & shift_op1(63),
    shift_op1
  ).asSInt >> shift_op2)(63, 0)
  val sll_res = Reverse(sra_srl_res)

  val shout = Mux1H(
    Seq(
      io.sra_req -> sra_srl_res,
      io.srl_req -> sra_srl_res,
      io.sll_req -> sll_res
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

object gen_adder_verilog extends App {
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new AluAdder(),
      Array("--target-dir", verilogDir)
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
