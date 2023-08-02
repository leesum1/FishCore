package leesum

import chisel3._
import chisel3.tester._
import chiseltest.ChiselScalatestTester
import leesum.RiscvTools.sign_ext
import org.scalatest.freespec.AnyFreeSpec

class SignExtModule(input_width: Int, output_width: Int) extends Module {
  val io = IO(new Bundle {
    val x = Input(UInt(input_width.W))
    val result = Output(UInt(output_width.W))
  })

  io.result := sign_ext(io.x, input_width, output_width)
}

class SignExtSpec extends AnyFreeSpec with ChiselScalatestTester {

  "Sign Extension should work correctly" in {
    test(new SignExtModule(32, 64)) { dut =>
      // Test cases
      val x =
        BigInt("ffffffff", 16) // 32-bit two's complement representation of -1

      dut.io.x.poke(x.U)

      // Calculate the expected result manually
      val expected = BigInt(
        "ffffffffffffffff",
        16
      ) // 64-bit two's complement representation of -1

      // Verify the output
      dut.io.result.expect(expected.U)

      // You can add more test cases for different scenarios
      // For example, test positive numbers, negative numbers, and other input/output widths

      // Test with a positive number
      val xPositive =
        BigInt("7ffffffe", 16) // 32-bit binary representation of 2147483646
      dut.io.x.poke(xPositive.U)

      // Calculate the expected result manually
      val expectedPositive = BigInt(
        "000000007ffffffe",
        16
      ) // 64-bit binary representation of 2147483646

      // Verify the output
      dut.io.result.expect(expectedPositive.U)

      // Add more test cases as needed
    }
  }
  "Sign Extension should work correctly8" in {
    test(new SignExtModule(8, 64)) { dut =>
      // Test cases
      val x = BigInt("ff", 16) // 8-bit two's complement representation of -1
      dut.io.x.poke(x.U)

      // Calculate the expected result manually
      val expected = BigInt(
        "ffffffffffffffff",
        16
      ) // 64-bit two's complement representation of -1

      // Verify the output
      dut.io.result.expect(expected.U)

      // You can add more test cases for different scenarios
      // For example, test positive numbers, negative numbers, and other input/output widths

      // Test with a positive number
      val xPositive = BigInt("7e", 16) // 8-bit binary representation of 126
      dut.io.x.poke(xPositive.U)

      // Calculate the expected result manually
      val expectedPositive =
        BigInt("000000000000007e", 16) // 64-bit binary representation of 126

      // Verify the output
      dut.io.result.expect(expectedPositive.U)

      // Add more test cases as needed
    }
  }
}
