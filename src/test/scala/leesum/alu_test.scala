package leesum

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import chiseltest.simulator.WriteVcdAnnotation

class AluTest extends AnyFreeSpec with ChiselScalatestTester {
  def alu_input_gen() = {
    val alu_op_gen =
      Gen.listOfN(16, Gen.hexChar).map("x" + _.mkString).map(_.U(64.W))

    val alu_op_list = Array(
      AluOP.Add,
      AluOP.Sub,
      AluOP.And,
      AluOP.Or,
      AluOP.Xor,
      AluOP.Sll,
      AluOP.Srl,
      AluOP.Sra,
      AluOP.Slt,
      AluOP.Sltu
    )

    val op_width_list = Array(OPWidth.W32, OPWidth.W64)

    val op_type_gen = Gen.choose(0, 9).map(alu_op_list(_))

    val op_width_gen = Gen.choose(0, 1).map(op_width_list(_))

    val input_gen = for {
      alu_op1 <- alu_op_gen
      alu_op2 <- alu_op_gen
      op_type <- op_type_gen
      op_width <- op_width_gen
    } yield {
      (alu_op1, alu_op2, op_type, op_width)
    }
    input_gen
  }

  // scala 没有无符号数比较，所以需要自己实现
  def longParseUnsigned(value: Long): BigDecimal = {
    if (value >= 0) {
      return BigDecimal(value)
    }
    val lowValue = value & Long.MaxValue

    BigDecimal
      .valueOf(lowValue) + BigDecimal.valueOf(Long.MaxValue) + BigDecimal
      .valueOf(1)
  }

  def alu_calculate_ref(
      op_a: UInt,
      op_b: UInt,
      op_type: AluOP.Type,
      op_width: OPWidth.Type
  ): Long = {
    require((op_a.getWidth == 64) && (op_b.getWidth == 64), "width error")

    val op_a_lit = op_a.litValue.toLong
    val op_b_lit = op_b.litValue.toLong

    val and_res = op_a_lit & op_b_lit
    val or_res = op_a_lit | op_b_lit
    val xor_res = op_a_lit ^ op_b_lit
    val add_res = op_a_lit + op_b_lit
    val sub_res = op_a_lit - op_b_lit

    val slt_res = BigInt(op_a_lit) < BigInt(op_b_lit)
    val sltu_res = longParseUnsigned(op_a_lit) < longParseUnsigned(op_b_lit)

    val res = (op_type, op_width) match {
      case (AluOP.Add, OPWidth.W64) => add_res
      case (AluOP.Sub, OPWidth.W64) => sub_res
      case (AluOP.And, OPWidth.W64) => and_res
      case (AluOP.Or, OPWidth.W64)  => or_res
      case (AluOP.Xor, OPWidth.W64) => xor_res
      case (AluOP.Sll, OPWidth.W64) => op_a_lit << (op_b_lit & 0x3f)
      case (AluOP.Srl, OPWidth.W64) => op_a_lit >>> (op_b_lit & 0x3f)
      case (AluOP.Sra, OPWidth.W64) => op_a_lit >> (op_b_lit & 0x3f)
      case (AluOP.Slt, _)           => if (slt_res) 1L else 0L
      case (AluOP.Sltu, _)          => if (sltu_res) 1L else 0L
      case (AluOP.Add, OPWidth.W32) => add_res.toInt.toLong // sign extend
      case (AluOP.Sub, OPWidth.W32) => sub_res.toInt.toLong // sign extend
      case (AluOP.And, OPWidth.W32) => and_res.toInt.toLong // sign extend
      case (AluOP.Or, OPWidth.W32)  => or_res.toInt.toLong // sign extend
      case (AluOP.Xor, OPWidth.W32) => xor_res.toInt.toLong // sign extend
      case (AluOP.Sll, OPWidth.W32) =>
        (op_a_lit.toInt << (op_b_lit & 0x1f)).toInt.toLong // sign extend
      case (AluOP.Srl, OPWidth.W32) =>
        (op_a_lit.toInt >>> (op_b_lit & 0x1f)).toInt.toLong // sign extend
      case (AluOP.Sra, OPWidth.W32) =>
        (op_a_lit.toInt >> (op_b_lit & 0x1f)).toInt.toLong
      case _ =>
        throw new Exception(
          "op_type:%d, op_width:%d not support".format(
            op_type.litValue,
            op_width.litValue
          )
        )
    }

    res
  }

  "ALU TEST" in {
    test(new FuAlu).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      val alu_gen = alu_input_gen()

      val input_seq = Gen
        .listOfN(2000, alu_gen)
        .sample
        .get

      val ref_seq = input_seq
        .map({ case (op_a, op_b, op_type, op_width) =>
          val res = alu_calculate_ref(op_a, op_b, op_type, op_width)
          "x" + res.toHexString
        })
        .map(_.U(64.W))
        .map({ case (res) =>
          (new AluOut).Lit(
            _.res -> res
          )
        })
      dut.io.in.initSource()
      dut.io.in.setSourceClock(dut.clock)
      dut.io.out.initSink()
      dut.io.out.setSinkClock(dut.clock)

      dut.clock.step(5)
      fork {
        // push inputs into the calculator, stall for 11 cycles one third of the way
        val (seq1, seq2) =
          input_seq
            .map({ case (op_a, op_b, op_type, op_width) =>
              (new AluIn).Lit(
                _.a -> op_a,
                _.b -> op_b,
                _.op -> op_type,
                _.rvw -> op_width
              )
            })
            .splitAt(input_seq.length / 3)

        dut.io.in.enqueueSeq(seq1)
        dut.clock.step(100)
        dut.io.in.enqueueSeq(seq2)
      }.fork {
        // retrieve computations from the calculator, stall for 10 cycles one half of the way
        val (seq1, seq2) = ref_seq.splitAt(ref_seq.length / 4)
        dut.io.out.expectDequeueSeq(seq1)
        dut.clock.step(100)
        dut.io.out.expectDequeueSeq(seq2)
      }.join()
    }
  }
}

class AluShiftTest extends AnyFreeSpec with ChiselScalatestTester {

  def shift_input_gen() = {
    val shift_op1_gen =
      Gen.listOfN(16, Gen.hexChar).map("x" + _.mkString).map(_.U(64.W))
    val shift_op2_gen = Gen.chooseNum(0, 63).map(_.U(6.W))

    val input_gen = for {
      shift_op1 <- shift_op1_gen
      shift_op2 <- shift_op2_gen
    } yield {
      (shift_op1, shift_op2)
    }
    input_gen
  }

  "SLL64 TEST" in {
    test(new AluShift).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      val sll64_gen = shift_input_gen()

      // use Gen.listOfN to generate a list of 100 items as input data
      val input_sll64_seq = Gen
        .listOfN(2000, sll64_gen)
        .sample
        .get
      val expect_sll64_seq =
        input_sll64_seq.map({ case (shift_op1, shift_op2) =>
          "x" + (shift_op1.litValue.toLong << shift_op2.litValue.toInt).toHexString
        })

      input_sll64_seq.zipWithIndex.foreach({
        case ((shift_op1, shift_op2), idx) =>
          dut.io.sll_req.poke(true.B)
          dut.io.srl_req.poke(false.B)
          dut.io.sra_req.poke(false.B)
          dut.io.shift32_req.poke(false.B)
          dut.io.shift_in.poke(shift_op1)
          dut.io.shift_count.poke(shift_op2)
          dut.io.shift_out.expect(expect_sll64_seq(idx).U)
          println(
            "sll64: shift_op1:%x, shift_op2:%x, dut_out:%x,ref_out:%s".format(
              shift_op1.litValue.toLong,
              shift_op2.litValue.toLong,
              dut.io.shift_out.peek().litValue.toLong,
              expect_sll64_seq(idx)
            )
          )
          dut.clock.step(1)
      })

    }
  }
  "SLL32 TEST" in {
    test(new AluShift).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      val sll32_gen = shift_input_gen()

      // use Gen.listOfN to generate a list of 100 items as input data
      val input_seq = Gen
        .listOfN(2000, sll32_gen)
        .sample
        .get
      val expect_seq = input_seq.map({
        case (shift_op1, shift_op2) => {
          "x" + (shift_op1.litValue.toInt << (shift_op2.litValue.toInt & 0x1f)).toHexString
        }
      })
//      expect_sll64_seq.foreach(println)

      input_seq.zipWithIndex.foreach({
        case ((shift_op1, shift_op2), idx) => {
          dut.io.sll_req.poke(true.B)
          dut.io.srl_req.poke(false.B)
          dut.io.sra_req.poke(false.B)
          dut.io.shift32_req.poke(true.B)
          dut.io.shift_in.poke(shift_op1)
          dut.io.shift_count.poke(shift_op2)
          dut.io.shift_out.expect(expect_seq(idx).U)
          println(
            "sll64: shift_op1:%x, shift_op2:%x, dut_out:%x,ref_out:%s".format(
              shift_op1.litValue.toLong,
              shift_op2.litValue.toLong,
              dut.io.shift_out.peek().litValue.toLong,
              expect_seq(idx)
            )
          )
          dut.clock.step(1)
        }
      })

    }
  }
  "SRL64 TEST" in {
    test(new AluShift).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      val srl64_gen = shift_input_gen()

      // use Gen.listOfN to generate a list of 100 items as input data
      val input_seq = Gen
        .listOfN(2000, srl64_gen)
        .sample
        .get

      val expect_seq = input_seq.map({
        case (shift_op1, shift_op2) => {
          "x" + (shift_op1.litValue.toLong >>> shift_op2.litValue.toInt).toHexString
        }
      })

      input_seq.zipWithIndex.foreach({
        case ((shift_op1, shift_op2), idx) => {
          dut.io.sll_req.poke(false.B)
          dut.io.srl_req.poke(true.B)
          dut.io.sra_req.poke(false.B)
          dut.io.shift32_req.poke(false.B)
          dut.io.shift_in.poke(shift_op1)
          dut.io.shift_count.poke(shift_op2)
          dut.io.shift_out.expect(expect_seq(idx).U)
          println(
            "srl64: shift_op1:%x, shift_op2:%x, dut_out:%x,ref_out:%s".format(
              shift_op1.litValue.toLong,
              shift_op2.litValue.toLong,
              dut.io.shift_out.peek().litValue.toLong,
              expect_seq(idx)
            )
          )
          dut.clock.step(1)
        }
      })

    }
  }
  "SRL32 TEST" in {
    test(new AluShift).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      val srl32_gen = shift_input_gen()

      // use Gen.listOfN to generate a list of 100 items as input data
      val input_seq = Gen
        .listOfN(2000, srl32_gen)
        .sample
        .get

      val expect_seq = input_seq.map({
        case (shift_op1, shift_op2) => {
          "x" + (shift_op1.litValue.toInt >>> (shift_op2.litValue.toInt & 0x1f)).toHexString
        }
      })

      input_seq.zipWithIndex.foreach({
        case ((shift_op1, shift_op2), idx) => {
          dut.io.sll_req.poke(false.B)
          dut.io.srl_req.poke(true.B)
          dut.io.sra_req.poke(false.B)
          dut.io.shift32_req.poke(true.B)
          dut.io.shift_in.poke(shift_op1)
          dut.io.shift_count.poke(shift_op2)
          dut.io.shift_out.expect(expect_seq(idx).U)
          println(
            "srl32: shift_op1:%x, shift_op2:%x, dut_out:%x,ref_out:%s".format(
              shift_op1.litValue.toLong,
              shift_op2.litValue.toLong,
              dut.io.shift_out.peek().litValue.toLong,
              expect_seq(idx)
            )
          )
          dut.clock.step(1)
        }
      })

    }
  }
  "SRA64 TEST" in {
    test(new AluShift).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      val sra64_gen = shift_input_gen()

      // use Gen.listOfN to generate a list of 100 items as input data
      val input_seq = Gen
        .listOfN(2000, sra64_gen)
        .sample
        .get

      val expect_seq = input_seq.map({
        case (shift_op1, shift_op2) => {
          "x" + (shift_op1.litValue.toLong >> shift_op2.litValue.toInt).toHexString
        }
      })

      input_seq.zipWithIndex.foreach({
        case ((shift_op1, shift_op2), idx) => {
          dut.io.sll_req.poke(false.B)
          dut.io.srl_req.poke(false.B)
          dut.io.sra_req.poke(true.B)
          dut.io.shift32_req.poke(false.B)
          dut.io.shift_in.poke(shift_op1)
          dut.io.shift_count.poke(shift_op2)
          dut.io.shift_out.expect(expect_seq(idx).U)
          println(
            "sra64: shift_op1:%x, shift_op2:%x, dut_out:%x,ref_out:%s".format(
              shift_op1.litValue.toLong,
              shift_op2.litValue.toLong,
              dut.io.shift_out.peek().litValue.toLong,
              expect_seq(idx)
            )
          )
          dut.clock.step(1)
        }
      })

    }
  }
  "SRA32 TEST" in {
    test(new AluShift).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      val sra32_gen = shift_input_gen()

      // use Gen.listOfN to generate a list of 100 items as input data
      val input_seq = Gen
        .listOfN(2000, sra32_gen)
        .sample
        .get

      val expect_seq = input_seq.map({
        case (shift_op1, shift_op2) => {
          "x" + (shift_op1.litValue.toInt >> (shift_op2.litValue.toInt & 0x1f)).toHexString
        }
      })

      input_seq.zipWithIndex.foreach({
        case ((shift_op1, shift_op2), idx) => {
          dut.io.sll_req.poke(false.B)
          dut.io.srl_req.poke(false.B)
          dut.io.sra_req.poke(true.B)
          dut.io.shift32_req.poke(true.B)
          dut.io.shift_in.poke(shift_op1)
          dut.io.shift_count.poke(shift_op2)
          dut.io.shift_out.expect(expect_seq(idx).U)
          println(
            "sra32: shift_op1:%x, shift_op2:%x, dut_out:%x,ref_out:%s".format(
              shift_op1.litValue.toLong,
              shift_op2.litValue.toLong,
              dut.io.shift_out.peek().litValue.toLong,
              expect_seq(idx)
            )
          )
          dut.clock.step(1)
        }
      })
    }
  }
}
