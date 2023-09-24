package leesum
import chisel3._
import chiseltest._
import leesum.axi4.AXIDef._
import leesum.axi4.{AXIAddr, axi_addr}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class RefDutCmp(ADDR_WIDTH: Int, DATA_WIDTH: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(ADDR_WIDTH.W))
    val len = Input(UInt(8.W))
    val size = Input(UInt(3.W))
    val burst = Input(UInt(2.W))
    val next_addr_dut = Output(UInt(ADDR_WIDTH.W))
    val next_addr_ref = Output(UInt(ADDR_WIDTH.W))
  })

  val ref = Module(new axi_addr(ADDR_WIDTH, DATA_WIDTH))
  val dut = Module(new AXIAddr(ADDR_WIDTH, DATA_WIDTH))

  ref.io.i_last_addr := io.addr
  ref.io.i_len := io.len
  ref.io.i_size := io.size
  ref.io.i_burst := io.burst
  io.next_addr_ref := ref.io.o_next_addr

  dut.io.addr := io.addr
  dut.io.len := io.len
  dut.io.size := io.size
  dut.io.burst := io.burst
  io.next_addr_dut := dut.io.next_addr
}
class AXI4AddrTest extends AnyFreeSpec with ChiselScalatestTester {
  def gen_rand_input(ADDR_WIDTH: Int) = {
    val gen_addr = Gen
      .listOfN(ADDR_WIDTH / 4, Gen.hexChar)
      .map("x" + _.mkString)
      .map(_.U(ADDR_WIDTH.W))
    val gen_size = Gen.oneOf(
      SIZE_1,
      SIZE_2,
      SIZE_4,
      SIZE_8
    )
    val input_seq_gen = for {
      addr <- gen_addr
      size <- gen_size
    } yield {
      val len = 4.U(8.W)
      val burst = BURST_INCR
      (addr, len, size, burst)
    }
    input_seq_gen
  }

  "axi_addr_test" in {
    test(new RefDutCmp(ADDR_WIDTH = 12, DATA_WIDTH = 64))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        val input_gen = gen_rand_input(ADDR_WIDTH = 12)
        val input_seq = Gen.listOfN(1000, input_gen).sample.get
        input_seq.foreach(input => {
          dut.io.addr.poke(input._1)
          dut.io.len.poke(input._2)
          dut.io.size.poke(input._3)
          dut.io.burst.poke(input._4)
          val ref_data = dut.io.next_addr_ref.peek()
          val dut_data = dut.io.next_addr_dut.peek()
          dut.clock.step(1)
          println(
            "addr:%x, len:%x, size:%x, burst:%x".format(
              input._1.litValue,
              input._2.litValue,
              input._3.litValue,
              input._4.litValue
            )
          )
          println("ref:%x, dut:%x".format(ref_data.litValue, dut_data.litValue))
          assert(ref_data.litValue == dut_data.litValue)
        })
      }
  }
}
