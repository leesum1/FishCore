package leesum

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.{TruthTable, decoder}
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import leesum.axi4.fsm_test
import org.scalatest.freespec.AnyFreeSpec

class FSMTest extends AnyFreeSpec with ChiselScalatestTester {
  "fsm_test" in {
    test(new fsm_test)
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) {
        dut =>
          {
            dut.clock.step(5)
            dut.io.in_valid.poke(true.B)
            dut.io.in_data.poke(1.U)
            // state idle
            dut.io.wire_out.expect((1 + 1).U)
            dut.io.reg_out.expect(0.U)

            dut.clock.step(1)
            dut.io.in_data.poke(2.U)
            // state idle
            dut.io.wire_out.expect((2 + 2).U)
            dut.io.reg_out.expect((1 + 1).U)

            dut.clock.step(1)
            dut.io.in_valid.poke(false.B)

            dut.io.reg_out.expect((2 + 2).U)
            dut.io.wire_out.expect((0).U)

            dut.clock.step(5)

          }
      }
  }
}
class SimpleDecoder extends Module {
  val table = TruthTable(
    Map(
      BitPat("b001") -> BitPat("b?"),
      BitPat("b010") -> BitPat("b?"),
      BitPat("b100") -> BitPat("b1"),
      BitPat("b101") -> BitPat("b1"),
      BitPat("b111") -> BitPat("b1")
    ),
    BitPat("b0")
  )
  val input = IO(Input(UInt(3.W)))
  val output = IO(Output(UInt(1.W)))
  output := decoder(input, table)
}

object gen_simple_decoder_verilog extends App {
  GenVerilogHelper(new SimpleDecoder)
}
