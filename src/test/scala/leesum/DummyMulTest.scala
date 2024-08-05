package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import chiseltest.simulator.WriteFstAnnotation
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import spire.math
import java.math.BigInteger

class MulRef {
  def mul(a: Long, b: Long): Long = {
    a * b
  }
  def mulw(a: Long, b: Long): Long = {
    TestUtils.sign_ext(a * b, 32)
  }
  def mulh(a: Long, b: Long): Long = {
    val a_big = new BigInteger(a.toString)
    val b_big = new BigInteger(b.toString)
    val ret = a_big.multiply(b_big).shiftRight(64)
    ret.longValue()
  }

  def mulhu(a: Long, b: Long): Long = {
    val a_u = math.ULong(a)
    val b_u = math.ULong(b)

    val a_big = new BigInteger(a_u.toString)
    val b_big = new BigInteger(b_u.toString)
    val ret = a_big.multiply(b_big).shiftRight(64)
    ret.longValue()
  }
  def mulhsu(a: Long, b: Long): Long = {
    val a_big = new BigInteger(a.toString)
    val b_u = math.ULong(b)

    val ret = a_big.multiply(b_u.bigInteger).shiftRight(64)
    ret.longValue()
  }
}

object ref_test extends App {
  val x: Long = Long.MaxValue
  val y = -1
  val x_u = math.ULong(x)
  val y_u = math.ULong(y)

  println(y_u.toString)

  val b_big = new BigInteger(y_u.toString)
  val a_big = new BigInteger(x_u.toString)

  println(a_big.multiply(b_big))
  println(Long.MaxValue)

}

class DummyMulTest extends AnyFreeSpec with ChiselScalatestTester {

  def gen_mul_req() = {
    val mul_op_gen = Gen.oneOf(
      FuOP.MulMul,
      FuOP.MulMulh,
      FuOP.MulMulhsu,
      FuOP.MulMulhu
    )
    for {
      op_a <- TestUtils.gen_rand_uint(64)
      op_b <- TestUtils.gen_rand_uint(64)
      op_type <- mul_op_gen
      is_rv32 <- Gen.oneOf(true, false)
    } yield {

      val x = op_type == FuOP.MulMul
      val rv32 = if (x) is_rv32 else false

      (new MulReq()).Lit(
        _.op_a -> op_a,
        _.op_b -> op_b,
        _.op_type -> op_type,
        _.is_rv32 -> rv32.B,
        _.trans_id -> 0.U
      )
    }
  }

  def gen_mul_resp(
      op_a: Long,
      op_b: Long,
//      op_type: FuOP.Type,
      op_type: UInt,
      is_rv32: Boolean
  ): MulResp = {
    val ref = new MulRef()

    val op_lit = op_type.litValue
    val mul_lit = FuOP.MulMul.litValue
    val mulh_lit = FuOP.MulMulh.litValue
    val mulhsu_lit = FuOP.MulMulhsu.litValue
    val mulhu_lit = FuOP.MulMulhu.litValue

    val ret = op_lit match {
      case `mul_lit` =>
        if (is_rv32) ref.mulw(op_a, op_b) else ref.mul(op_a, op_b)
      case `mulh_lit`   => ref.mulh(op_a, op_b)
      case `mulhsu_lit` => ref.mulhsu(op_a, op_b)
      case `mulhu_lit`  => ref.mulhu(op_a, op_b)
      case _ =>
        assert(false, "op_type is not mul")
        0L // Return a default value when the assertion fails
    }

    (new MulResp).Lit(_.data -> TestUtils.long2UInt64(ret), _.trans_id -> 0.U)
  }

  "DummyMulTest" in {
    test(new DummyMul()).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
    ) { dut =>
      // -----------------------
      // init port
      // -----------------------
      dut.io.mul_req.initSource().setSourceClock(dut.clock)
      dut.io.mul_resp.initSink().setSinkClock(dut.clock)
      dut.io.flush.poke(false.B)
      dut.clock.step(5)
      // -----------------------
      // prepare test data
      // -----------------------

      val mul_req_seq = Gen.listOfN(100000, gen_mul_req()).sample.get
      val mul_resp_seq = mul_req_seq.map(req => {
        gen_mul_resp(
          req.op_a.litValue.toLong,
          req.op_b.litValue.toLong,
          req.op_type,
          req.is_rv32.litValue == 1
        )
      })

      // -----------------------
      // test without bubble
      // -----------------------

      fork {
        dut.io.mul_req.enqueueSeq(mul_req_seq)
      }.fork {
        dut.io.mul_resp.expectDequeueSeq(mul_resp_seq)
      }.joinAndStep(dut.clock)

    }
  }
}
