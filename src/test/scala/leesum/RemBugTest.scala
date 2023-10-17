package leesum
import chisel3._
import chisel3.util.MuxLookup
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class RemBug extends Module {
  val io = IO(new Bundle {
    val op_a = Input(UInt(64.W))
    val op_b = Input(UInt(64.W))
    val op_type = Input(UInt(3.W))
    val res = Output(UInt(64.W))
  })

  val rem_res = (io.op_a.asSInt % io.op_b.asSInt)
  val div_res = (io.op_a.asSInt / io.op_b.asSInt)

  dontTouch(rem_res)

  val res = MuxLookup(io.op_type, 0.U(64.W))(
    Seq(
      1.U -> div_res(63, 0),
      2.U -> rem_res.asUInt
    )
  )

  io.res := res
}

object gen_rem_bug_verilog extends App {
  GenVerilogHelper(new RemBug)
}

class RemBugTest extends AnyFreeSpec with ChiselScalatestTester {

  "RemBugTest1" in {
    test(new RemBug)
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        val op_a = TestUtils.long2UInt64(0xffff_ffff_ffff_fffeL)
        val op_b = TestUtils.long2UInt64(0xffff_ffef_ffff_ffffL)

        dut.io.op_a.poke(op_a)
        dut.io.op_b.poke(op_b)
        dut.io.op_type.poke(2.U)
        val res = dut.io.res.peek()
        println(0xffff_ffff_ffff_fffeL % 0xffff_ffef_ffff_ffffL)
        println(res.litValue.longValue)

      }
  }
}
