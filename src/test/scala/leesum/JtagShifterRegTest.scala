package leesum
import chisel3._
import chiseltest.{ChiselScalatestTester, _}
import leesum.dbg.JtagShiftReg
import org.scalatest.freespec.AnyFreeSpec

class JtagShifterRegTest extends AnyFreeSpec with ChiselScalatestTester {
  "JtagShifterRegTest" in {
    test(
      new JtagShiftReg(32)
    )
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.update_valid.poke(true.B)
        dut.io.update_data.poke(0x12345678.U)
        dut.io.update_data_len.poke(32.U)
        dut.clock.step(1)
        dut.io.update_valid.poke(false.B)

        def set_new_shift_reg(new_val: UInt, new_len: UInt): Unit = {
          dut.io.update_valid.poke(true.B)
          dut.io.update_data.poke(new_val)
          dut.io.update_data_len.poke(new_len)
          dut.clock.step(1)
          dut.io.update_valid.poke(false.B)
        }

        def shift_in_and_out(
            shift_in_value: Long,
            shift_out_value: Long,
            shift_len: Int
        ): Unit = {
          var cur_shift_in_value = shift_in_value
          var cur_shift_out_value = shift_out_value
          for (i <- 0 until shift_len) {
            dut.io.shift_out.expect((cur_shift_out_value & 1) == 1)
            cur_shift_out_value = cur_shift_out_value >>> 1

            dut.io.shift_in.poke((cur_shift_in_value & 1) == 1)
            cur_shift_in_value = cur_shift_in_value >>> 1

            dut.io.shift_valid.poke(true.B)
            dut.clock.step(1)

          }
          dut.io.shift_valid.poke(false.B)
        }

        // Test 1: shift in 0x12345678, shift out 0x00000000
        set_new_shift_reg(0x00000000.U, 32.U)
        shift_in_and_out(0x12345678, 0x00000000, 32)

        // Test 2: shift in 0x12345678, shift out 0x00000001
        set_new_shift_reg(0x00000001.U, 32.U)
        shift_in_and_out(0x12345678, 0x00000001, 32)

        // Test 3: shift in 0xdead, shift out 0xbeef
        set_new_shift_reg(0xbeef.U, 16.U)
        shift_in_and_out(0xdead, 0xbeef, 16)

        // Test 4: shift in 0xf, shift out 0x0
        set_new_shift_reg(0x0.U, 4.U)
        dut.clock.step(10)
        shift_in_and_out(0xf, 0x0, 4)

        dut.clock.step(10)
      }
  }
}
