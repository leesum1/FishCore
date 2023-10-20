package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import leesum.ExtendedDecoupledDriverImplicits.toExtendedDriver
import leesum.TestUtils.{gen_rand_uint, long2UInt64}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
class FuCsrTest extends AnyFreeSpec with ChiselScalatestTester {
//  class FuCsrReq extends Bundle {
//    val csr_addr = UInt(12.W)
//    val rs1_or_zimm = UInt(64.W) // zimm or rs1_data
//    val only_read = Bool() // only read, no write, rs1 == x0, or zimm = 0
//    val trans_id = UInt(32.W)
//    val csr_op = FuOP()
//  }

  def gen_csr_req(): Gen[FuCsrReq] = {
    val gen_csr_op = Gen.oneOf(
      Seq(
        FuOP.CSRRS,
        FuOP.CSRRC,
        FuOP.CSRRW
      )
    )
    for {
      csr_op <- gen_csr_op
      csr_addr <- gen_rand_uint(12)
      only_read <- Gen.oneOf(true.B, false.B)
      trans_id <- gen_rand_uint(32)
      rs1_or_zimm <- gen_rand_uint(64)
    } yield {
      (new FuCsrReq).Lit(
        _.csr_addr -> csr_addr,
        _.rs1_or_zimm -> rs1_or_zimm,
        _.csr_op -> csr_op,
        _.only_read -> only_read,
        _.trans_id -> trans_id
      )
    }
  }

//  class FuCsrResp extends Bundle {
//    val data = UInt(64.W)
//    val trans_id = UInt(32.W)
//    val exception = new ExceptionEntry
//  }

  def gen_csr_resp(req: FuCsrReq) = {
    val csr_data = req.csr_addr.litValue.toLong

    (new FuCsrResp).Lit(
      _.trans_id -> req.trans_id,
      _.data -> TestUtils.long2UInt64(csr_data),
      _.exception.valid -> false.B,
      _.exception.cause -> ExceptionCause.unknown,
      _.exception.tval -> 0.U
    )
  }

  def get_csr_wdata(req: FuCsrReq): UInt = {
    val csr_data = req.csr_addr.litValue.toLong
    val offset = req.csr_addr.litValue.toLong & 0x3f

    val op_csrrs = FuOP.CSRRS.litValue
    val op_csrrc = FuOP.CSRRC.litValue
    val op_csrrw = FuOP.CSRRW.litValue

    val result = req.csr_op.litValue match {
      case `op_csrrs` =>
        csr_data | (1L << offset)
      case `op_csrrc` =>
        csr_data & ~(1L << offset)
      case `op_csrrw` =>
        req.rs1_or_zimm.litValue.toLong
    }

    long2UInt64(result)
  }

  "FuCsrTest1" in {
    test(new FuCSR)
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) {
        dut =>
          // -----------------------
          // init port
          // -----------------------
          init_port(dut)

          // -----------------------
          // prepare test data
          // -----------------------

          val req_seq = Gen.listOfN(1000, gen_csr_req()).sample.get
          val resp_seq = req_seq.map(gen_csr_resp)

          // -----------------------
          // test1
          // -----------------------
          req_seq.foreach(req => {
            dut.io.csr_rw_port.read_data.poke(req.csr_addr)
            dut.io.csr_rw_port.write_exception.poke(false.B)
            dut.io.csr_req.enqueue(req)

            fork {
              dut.io.csr_commit.enqueue(true.B)
            }.fork {
              dut.io.csr_resp
                .expectDequeueAdditional(
                  resp_seq(req_seq.indexOf(req)),
                  dut.clock
                ) {
                  dut.io.csr_rw_port.write_data.expect(get_csr_wdata(req))
                }
            }.joinAndStep(dut.clock)

          })
          dut.clock.step(5)
          // -----------------------
          // test2
          // -----------------------

          fork {
            dut.io.csr_req.enqueueSeq(req_seq)
          }.fork {
            dut.io.csr_commit.enqueueSeq(Seq.fill(req_seq.length)(true.B))
          }.fork {
            req_seq.foreach(req => {
              dut.io.csr_rw_port.read_data.poke(req.csr_addr)
              dut.io.csr_rw_port.write_exception.poke(false.B)
              dut.io.csr_resp
                .expectDequeueAdditional(
                  resp_seq(req_seq.indexOf(req)),
                  dut.clock
                ) {
                  dut.io.csr_rw_port.write_data.expect(get_csr_wdata(req))
                }
            })
          }.joinAndStep(dut.clock)

          dut.clock.step(5)
      }
  }

  private def init_port(dut: FuCSR): Unit = {
    dut.io.csr_req.initSource().setSourceClock(dut.clock)
    dut.io.csr_resp.initSink().setSinkClock(dut.clock)
    dut.io.csr_commit.initSource().setSourceClock(dut.clock)
    dut.io.flush.poke(false.B)
    dut.io.csr_rw_port.read_data.poke(0.U)
    dut.io.csr_rw_port.write_exception.poke(false.B)
    dut.clock.step(5)
  }
}
