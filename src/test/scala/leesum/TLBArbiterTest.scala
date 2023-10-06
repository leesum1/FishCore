package leesum
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.Decoupled
import leesum.TestUtils.gen_rand_uint
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class TLBArbiterTestDut(num_input: Int) extends Module {
  val io = IO(new Bundle {
    val in_req = Vec(num_input, Flipped(Decoupled(new TLBReq)))
    val in_resp = Vec(num_input, Decoupled(new TLBResp))
  })

  val tlb = Module(new DummyTLB)
  val arb = Module(new TLBArbiter(num_input))

  arb.io.in_req.zip(io.in_req).foreach { case (arb_in, io_in) =>
    arb_in <> io_in
  }
  arb.io.in_resp.zip(io.in_resp).foreach { case (arb_in, io_in) =>
    arb_in <> io_in
  }

  arb.io.out_req <> tlb.io.tlb_req
  arb.io.out_resp <> tlb.io.tlb_resp

  tlb.io.flush := false.B
}

object gen_TLBArbiterTest_verilog extends App {
  GenVerilogHelper(new TLBArbiterTestDut(2))
}

class TLBArbiterTest extends AnyFreeSpec with ChiselScalatestTester {

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

  def gen_tlb_resp(req: TLBReq) = {
    (new TLBResp).Lit(
      _.paddr -> req.vaddr,
      _.req_type -> req.req_type,
      _.size -> req.size,
      _.exception.valid -> false.B,
      _.exception.tval -> 0.U,
      _.exception.cause -> ExceptionCause.load_access
    )
  }

  "TLBArbiterTest1" in {
    test(new TLBArbiterTestDut(2))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // ------------------
        // init port
        // ------------------
        dut.io.in_req.foreach(_.initSource().setSourceClock(dut.clock))
        dut.io.in_resp.foreach(_.initSink().setSinkClock(dut.clock))
        dut.clock.step(5)

        // ------------------
        // prepare test data
        // ------------------

        val tlb1_req_seq = Gen.listOfN(100, gen_tlb_req).sample.get
        val tlb2_req_seq = Gen.listOfN(100, gen_tlb_req).sample.get
        val tlb1_resp_seq = tlb1_req_seq.map(gen_tlb_resp)
        val tlb2_resp_seq = tlb2_req_seq.map(gen_tlb_resp)

        // ------------------
        // run test1
        // ------------------

        fork {
          dut.io.in_req(0).enqueueSeq(tlb1_req_seq)
        }.fork {
          dut.io.in_resp(0).expectDequeueSeq(tlb1_resp_seq)
        }.fork {
          dut.io.in_req(1).enqueueSeq(tlb2_req_seq)
        }.fork {
          dut.io.in_resp(1).expectDequeueSeq(tlb2_resp_seq)
        }.joinAndStep(dut.clock)

        dut.clock.step(5)
        // ------------------
        // run test2
        // ------------------

        fork {
          dut.io.in_req(0).enqueueSeq(tlb1_req_seq)
          dut.io.in_req(1).enqueueSeq(tlb2_req_seq)

        }.fork {
          dut.io.in_resp(0).expectDequeueSeq(tlb1_resp_seq)
          dut.io.in_resp(1).expectDequeueSeq(tlb2_resp_seq)
        }.joinAndStep(dut.clock)

        dut.clock.step(5)

        // ------------------
        // run test3
        // ------------------

        fork {
          tlb1_req_seq.foreach(req => {
            dut.io.in_req(0).enqueue(req)
            dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }.fork {
          tlb1_resp_seq.foreach(resp => {
            dut.io.in_resp(0).expectDequeue(resp)
            dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }.fork {
          tlb2_req_seq.foreach(req => {
            dut.io.in_req(1).enqueue(req)
            dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }.fork {
          tlb2_resp_seq.foreach(resp => {
            dut.io.in_resp(1).expectDequeue(resp)
            dut.clock.step(Gen.chooseNum(1, 10).sample.get)
          })
        }.joinAndStep(dut.clock)

        dut.clock.step(5)

      }
  }
}
