package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import leesum.test_utils.gen_rand_uint
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class DummyTLBTest extends AnyFreeSpec with ChiselScalatestTester {

  def gen_tlb_req = {
    val vaddr = gen_rand_uint(64)
    val size = gen_rand_uint(2)
    val tlb_type = Gen.oneOf(TLBReqType.LOAD, TLBReqType.STORE)
    val tlb_req = for {
      vaddr <- vaddr
      tlb_type <- tlb_type
      size <- size
    } yield {
      (new TLBReq).Lit(_.vaddr -> vaddr, _.req_type -> tlb_type, _.size -> size)
    }
    tlb_req
  }

  "DummyTLBTest1" in {
    test(new DummyTLB)
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.tlb_resp.initSink().setSinkClock(dut.clock)
        dut.io.tlb_req.initSource().setSourceClock(dut.clock)

        val input_seq = Gen.listOfN(100, gen_tlb_req).sample.get
        val output_req = input_seq.map((req => {
          (new TLBResp).Lit(
            _.paddr -> req.vaddr,
            _.req_type -> req.req_type,
            _.size -> req.size,
            _.exception.valid -> false.B,
            _.exception.tval -> 0.U,
            _.exception.cause -> ExceptionCause.load_access
          )
        }))

        dut.clock.step(5)
        fork {
          dut.io.tlb_req.enqueueSeq(input_seq)
        }.fork {
          dut.clock.step(10)
          dut.io.tlb_resp.expectDequeueSeq(output_req)
        }.joinAndStep(dut.clock)
        dut.clock.step(5)

        fork {
          input_seq.foreach(req => {
            dut.clock.step(10)
            dut.io.tlb_req.enqueue(req)
          })
        }.fork {
          dut.clock.step(10)
          output_req.foreach(resp => {
            dut.clock.step(5)
            dut.io.tlb_resp.expectDequeue(resp)
          })
        }.joinAndStep(dut.clock)
        dut.clock.step(5)
      }
  }
}
