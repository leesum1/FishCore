package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.Decoupled
import chiseltest._
import leesum.test_utils.{check_aligned, gen_rand_uint, long2UInt64}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

import scala.language.postfixOps

class AGU_dut extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AGUIn))
    val out = new AGUOut
    val flush = Input(Bool())
  })

  val agu = Module(new AGU)
  val tlb = Module(new DummyTLB)

  agu.io.in <> io.in
  agu.io.out <> io.out
  agu.io.tlb_req <> tlb.io.tlb_req
  agu.io.tlb_resp <> tlb.io.tlb_resp
  tlb.io.flush := io.flush
  agu.io.flush := io.flush
}

object gen_agu_dut_verilog extends App {
  GenVerilogHelper(new AGU_dut())
}

class AGUTest extends AnyFreeSpec with ChiselScalatestTester {

  def gen_agu_in_input(): Gen[AGUIn] = {
    val op_a = gen_rand_uint(64)
    val op_b = gen_rand_uint(64)
    val size = gen_rand_uint(2)
    val store_data = gen_rand_uint(64)
    val trans_id = gen_rand_uint(32)
    val is_store = Gen.oneOf(true.B, false.B)

    val input_gen = for {
      op_a <- op_a
      op_b <- op_b
      size <- size
      store_data <- store_data
      is_store <- is_store
      trans_id <- trans_id
    } yield {
      (new AGUIn).Lit(
        _.op_a -> op_a,
        _.op_b -> 0.U,
        _.size -> size,
        _.store_data -> store_data,
        _.is_store -> is_store,
        _.trans_id -> trans_id
      )
    }
    input_gen
  }

  "AGU_test1" in {
    test(new AGU_dut)
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.in.initSource().setSourceClock(dut.clock)
        dut.io.out.load_pipe.initSink().setSinkClock(dut.clock)
        dut.io.out.store_pipe.initSink().setSinkClock(dut.clock)
        dut.io.out.exception_pipe.initSink().setSinkClock(dut.clock)
        dut.clock.step(5)

        val input_seq = Gen.listOfN(1000, gen_agu_in_input()).sample.get

        // if addr is aligned, then generate load or store
        val output_load_seq = input_seq
          .filter(input => {
            !input.is_store.litToBoolean && check_aligned(
              (input.op_a.litValue + input.op_b.litValue).toLong,
              input.size.litValue.toInt
            )
          })
          .map(input => {
            (new LoadQueueIn).Lit(
              _.paddr -> long2UInt64(
                (input.op_a.litValue + input.op_b.litValue).toLong
              ),
              _.size -> input.size,
              _.is_mmio -> false.B,
              _.trans_id -> input.trans_id
            )
          })
        // if addr is aligned, then generate load or store
        val output_store_seq = input_seq
          .filter(input => {
            input.is_store.litToBoolean && check_aligned(
              (input.op_a.litValue + input.op_b.litValue).toLong,
              input.size.litValue.toInt
            )
          })
          .map(input => {
            (new StoreQueueIn).Lit(
              _.paddr -> long2UInt64(
                (input.op_a.litValue + input.op_b.litValue).toLong
              ),
              _.store_data -> input.store_data,
              _.size -> input.size,
              _.is_mmio -> false.B,
              _.trans_id -> input.trans_id
            )
          })

        // if addr is not aligned, then generate exception
        val output_exception_seq = input_seq
          .filter(input => {
            !check_aligned(
              (input.op_a.litValue + input.op_b.litValue).toLong,
              input.size.litValue.toInt
            )
          })
          .map(input => {
            val ex = (new ExceptionEntry).Lit(
              _.valid -> true.B,
              _.tval -> long2UInt64(
                (input.op_a.litValue + input.op_b.litValue).toLong
              ),
              _.cause -> (if (input.is_store.litToBoolean) {
                            ExceptionCause.misaligned_store
                          } else {
                            ExceptionCause.misaligned_load
                          })
            )

            (new ExceptionQueueIn).Lit(
              _.trans_id -> input.trans_id,
              _.exception -> ex
            )
          })

        dut.clock.step(5)
        // back to back,no bubble
        fork {
          dut.io.in.enqueueSeq(input_seq)
        }.fork {
          dut.io.out.load_pipe.expectDequeueSeq(output_load_seq)
        }.fork {
          dut.io.out.store_pipe.expectDequeueSeq(output_store_seq)
        }.fork {
          dut.io.out.exception_pipe.expectDequeueSeq(output_exception_seq)
        }.joinAndStep(dut.clock)

        dut.clock.step(5)
        // with bubble
        fork {
          input_seq.foreach(input => {
            dut.io.in.enqueue(input)
            dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }.fork {
          output_load_seq.foreach(output => {
            dut.io.out.load_pipe.expectDequeue(output)
            dut.clock.step(Gen.chooseNum(4, 15).sample.get)
          })
        }.fork {
          output_store_seq.foreach(output => {
            dut.io.out.store_pipe.expectDequeue(output)
            dut.clock.step(Gen.chooseNum(1, 4).sample.get)
          })
        }.fork {
          output_exception_seq.foreach(output => {
            dut.io.out.exception_pipe.expectDequeue(output)
            dut.clock.step(Gen.chooseNum(1, 4).sample.get)
          })
        }.joinAndStep(dut.clock)
      }
  }
}
