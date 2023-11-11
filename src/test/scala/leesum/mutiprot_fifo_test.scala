//package leesum
//import chisel3._
//import chiseltest._
//import leesum.TestUtils.int2UInt32
//import org.scalatest.freespec.AnyFreeSpec
//
//import scala.util.Random
//class mutiprot_fifo_test extends AnyFreeSpec with ChiselScalatestTester {
//
//  def push_data_fail(
//      dut: MultiportFIFO[UInt],
//      nums: Int
//  ) = timescope {
//    for (i <- 0 until nums) {
//      dut.io.push_data(i).poke(int2UInt32(Random.nextInt()))
//      dut.io.push_valid(i).poke(Random.nextBoolean().B)
//    }
//    dut.clock.step(1)
//  }
//  def push_data(
//      dut: MultiportFIFO[UInt],
//      nums: Int
//  ) = timescope {
//    for (i <- 0 until nums) {
//      dut.io.push_data(i).poke(int2UInt32(Random.nextInt()))
//      dut.io.push_valid(i).poke(true.B)
//    }
//    dut.clock.step(1)
//  }
//  def pop_data(
//      dut: MultiportFIFO[UInt],
//      nums: Int
//  ) = timescope {
//    for (i <- 0 until nums) {
//      dut.io.pop_valid(i).poke(true.B)
//    }
//    dut.clock.step(1)
//  }
//
//  "mutiportfifo_test" in {
//    test(new MultiportFIFO(UInt(32.W), 8, 4, 4))
//      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) {
//        dut =>
//          {
//            dut.clock.step(5)
//            push_data(dut, 4)
//            dut.clock.step(5)
//            push_data(dut, 2)
//            pop_data(dut, 2)
//            dut.clock.step(5)
//            pop_data(dut, 2)
//            dut.clock.step(5)
//          }
//      }
//  }
//}
