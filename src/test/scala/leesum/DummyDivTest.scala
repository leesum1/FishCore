package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import spire.math

class div_ref {
  def div(a: Long, b: Long): Long = {
    if (b == 0) {
      -1
    } else if (a == Long.MinValue && b == -1) {
      Long.MinValue
    } else {
      a / b
    }
  }
  def rem(a: Long, b: Long): Long = {
    if (b == 0) {
      a
    } else if (a == Long.MinValue && b == -1) {
      0
    } else {
      a % b
    }
  }
  def divu(a: Long, b: Long): Long = {
    if (b == 0) {
      0xffffffffffffffffL
    } else {
      (math.ULong(a) / math.ULong(b)).toLong
    }
  }
  def remu(a: Long, b: Long): Long = {
    if (b == 0) {
      a
    } else {
      (math.ULong(a) % math.ULong(b)).toLong
    }
  }

  def divw(a: Long, b: Long): Long = {
    val op_a = (a & 0xffffffffL).toInt
    val op_b = (b & 0xffffffffL).toInt

    val ret = if (op_b == 0) {
      -1
    } else if (op_a == Int.MinValue && op_b == -1) {
      Int.MinValue
    } else {
      op_a / op_b
    }
    TestUtils.sign_ext(ret, 32)
  }

  def remw(a: Long, b: Long): Long = {
    val op_a = (a & 0xffffffffL).toInt
    val op_b = (b & 0xffffffffL).toInt

    val ret = if (op_b == 0) {
      op_a
    } else if (op_a == Int.MinValue && op_b == -1) {
      0
    } else {
      op_a % op_b
    }
    TestUtils.sign_ext(ret, 32)
  }

  def divuw(a: Long, b: Long): Long = {
    val op_a = (a & 0xffffffffL)
    val op_b = (b & 0xffffffffL)

    val ret = if (op_b == 0) {
      0xffffffffL
    } else {
      op_a / op_b
    }
    TestUtils.sign_ext(ret, 32)
  }

  def remuw(a: Long, b: Long): Long = {
    val op_a = (a & 0xffffffffL)
    val op_b = (b & 0xffffffffL)

    val ret = if (op_b == 0) {
      op_a
    } else {
      op_a % op_b
    }
    TestUtils.sign_ext(ret, 32)
  }

}

object div_ref_test extends App {

  val ref = new div_ref()

  val x1 = ref.remu(0xffff_ffff_ffff_fffeL, 0xffff_ffef_ffff_ffffL)
  println(x1.toHexString)

}

class DummyDivTest extends AnyFreeSpec with ChiselScalatestTester {
  def gen_div_req() = {
    val div_op_gen = Gen.oneOf(
      FuOP.DivDiv,
      FuOP.DivDivu,
      FuOP.DivRem,
      FuOP.DivRemu
    )
    for {
      op_a <- TestUtils.gen_rand_uint(64)
      op_b <- TestUtils.gen_rand_uint(64)
      op_type <- div_op_gen
      is_rv32 <- Gen.oneOf(true.B, false.B)
    } yield {
      (new DivReq).Lit(
        _.op_a -> op_a,
        _.op_b -> op_b,
        _.op_type -> op_type,
        _.is_rv32 -> is_rv32,
        _.trans_id -> 0.U
      )
    }
  }

  def gen_div_resp(div_req: DivReq) = {
    val ref = new div_ref()

    val op_a = div_req.op_a.litValue.toLong
    val op_b = div_req.op_b.litValue.toLong
    val is_rv32 = div_req.is_rv32.litToBoolean

    val op_lit = div_req.op_type.litValue
    val div_lit = FuOP.DivDiv.litValue
    val divu_lit = FuOP.DivDivu.litValue
    val rem_lit = FuOP.DivRem.litValue
    val remu_lit = FuOP.DivRemu.litValue

    val ret = op_lit match {
      case `div_lit` =>
        if (is_rv32) {
          ref.divw(op_a, op_b)
        } else {
          ref.div(op_a, op_b)
        }
      case `divu_lit` => {
        if (is_rv32) {
          ref.divuw(op_a, op_b)
        } else {
          ref.divu(op_a, op_b)
        }
      }
      case `rem_lit` => {
        if (is_rv32) {
          ref.remw(op_a, op_b)
        } else {
          ref.rem(op_a, op_b)
        }
      }
      case `remu_lit` => {
        if (is_rv32) {
          ref.remuw(op_a, op_b)
        } else {
          ref.remu(op_a, op_b)
        }
      }
    }

    (new DivResp).Lit(
      _.data -> TestUtils.long2UInt64(ret),
      _.trans_id -> div_req.trans_id
    )
  }

  "DummyDivTest" in {
    test(new DummyDiv()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
    ) { dut =>
      // -----------------------
      // init port
      // -----------------------
      dut.io.div_req.initSource().setSourceClock(dut.clock)
      dut.io.div_resp.initSink().setSinkClock(dut.clock)
      dut.io.flush.poke(false.B)
      dut.clock.step(5)
      // -----------------------
      // prepare test data
      // -----------------------

      val div_req_seq = Gen.listOfN(100000, gen_div_req()).sample.get
      val div_resp_seq = div_req_seq.map(req => {
        gen_div_resp(req)
      })

      // -----------------------
      // test without bubble
      // -----------------------

      fork {
        dut.io.div_req.enqueueSeq(div_req_seq)
      }.fork {
        dut.io.div_resp.expectDequeueSeq(div_resp_seq)
      }.joinAndStep(dut.clock)
    }
  }
}
