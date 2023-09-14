package leesum

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import leesum.test_utils.{int2UInt64, long2UInt64, long2Ulong}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class AluAdderTest extends AnyFreeSpec with ChiselScalatestTester {
  def adder_input_gen() = {
    val adder_op_gen =
      Gen.listOfN(16, Gen.hexChar).map("x" + _.mkString).map(_.U(64.W))
    val input_gen = for {
      adder_op1 <- adder_op_gen
      adder_op2 <- adder_op_gen
    } yield {
      (adder_op1, adder_op2)
    }
    input_gen
  }
  def adder_calculate_ref(op_a: UInt, op_b: UInt) = {
    require((op_a.getWidth == 64) && (op_b.getWidth == 64), "width error")

    val op_a_lit = op_a.litValue.toLong
    val op_b_lit = op_b.litValue.toLong

    val add_res = op_a_lit + op_b_lit
    val sub_res = op_a_lit - op_b_lit
    val slt_res = op_a_lit < op_b_lit
    val sltu_res = long2Ulong(op_a_lit) < long2Ulong(op_b_lit)

    (add_res, sub_res, slt_res, sltu_res)
  }

  "ADDER SUB TEST" in {
    test(new AluAdder).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      val input_gen = adder_input_gen()
      val input_seq = Gen
        .listOfN(20000, input_gen)
        .sample
        .get

      val ref_seq = input_seq
        .map({ case (op_a, op_b) =>
          val ref =
            adder_calculate_ref(op_a, op_b)
          ref
        })
      dut.clock.step(5)

      input_seq.zipWithIndex.foreach({ case ((op_a, op_b), idx) =>
        val (add_res, sub_res, slt_res, sltu_res) = ref_seq(idx)

        dut.io.adder_in1.poke(op_a)
        dut.io.adder_in2.poke(op_b)
        dut.io.sub_req.poke(true.B)

//        println(
//          "sub: op_a:%x, op_b:%x, dut_out:%x,ref_out:%x,dut_slt:%b,ref_slt:%b,dut_sltu:%b,ref_sltu:%b"
//            .format(
//              op_a.litValue.toLong,
//              op_b.litValue.toLong,
//              dut.io.adder_out.peek().litValue,
//              sub_res,
//              dut.io.slt.peek().litToBoolean,
//              slt_res,
//              dut.io.sltu.peek().litToBoolean,
//              sltu_res
//            )
//        )

        dut.io.adder_out.expect(long2UInt64(sub_res))
//        dut.io.slt.expect(slt_res)
//        dut.io.sltu.expect(sltu_res)

        dut.clock.step(1)
      })
    }
  }
}

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

    val op_width_list = Array(true.B, false.B)

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

  /** calculate the reference result of alu
    * @param op_a
    *   alu input a 64bit
    * @param op_b
    *   alu input b 64bit
    * @param op_type
    *   operation type
    * @param op_width
    *   input width 32bit or 64bit
    * @return
    */
  def alu_calculate_ref(
      op_a: UInt,
      op_b: UInt,
      op_type: AluOP.Type,
      is_rv32: Bool
  ): Long = {
    require((op_a.getWidth == 64) && (op_b.getWidth == 64), "width error")

    val op_a_lit = op_a.litValue.toLong
    val op_b_lit = op_b.litValue.toLong

    val and_res = op_a_lit & op_b_lit
    val or_res = op_a_lit | op_b_lit
    val xor_res = op_a_lit ^ op_b_lit
    val add_res = op_a_lit + op_b_lit
    val sub_res = op_a_lit - op_b_lit

    val slt_res = op_a_lit < op_b_lit
    val sltu_res = long2Ulong(op_a_lit) < long2Ulong(op_b_lit)
    val res = (op_type, is_rv32.litToBoolean) match {
      case (AluOP.Add, false) => add_res
      case (AluOP.Sub, false) => sub_res
      case (AluOP.And, false) => and_res
      case (AluOP.Or, false)  => or_res
      case (AluOP.Xor, false) => xor_res
      case (AluOP.Sll, false) => op_a_lit << (op_b_lit & 0x3f)
      case (AluOP.Srl, false) => op_a_lit >>> (op_b_lit & 0x3f)
      case (AluOP.Sra, false) => op_a_lit >> (op_b_lit & 0x3f)
      case (AluOP.Slt, _)     => if (slt_res) 1L else 0L
      case (AluOP.Sltu, _)    => if (sltu_res) 1L else 0L
      case (AluOP.Add, true)  => add_res.toInt.toLong // sign extend
      case (AluOP.Sub, true)  => sub_res.toInt.toLong // sign extend
      case (AluOP.And, true)  => and_res.toInt.toLong // sign extend
      case (AluOP.Or, true)   => or_res.toInt.toLong // sign extend
      case (AluOP.Xor, true)  => xor_res.toInt.toLong // sign extend
      case (AluOP.Sll, true) =>
        (op_a_lit.toInt << (op_b_lit & 0x1f)).toInt.toLong // sign extend
      case (AluOP.Srl, true) =>
        (op_a_lit.toInt >>> (op_b_lit & 0x1f)).toInt.toLong // sign extend
      case (AluOP.Sra, true) =>
        (op_a_lit.toInt >> (op_b_lit & 0x1f)).toInt.toLong
      case _ =>
        throw new Exception(
          "op_type:%d, op_width:%d not support".format(
            op_type.litValue,
            is_rv32.litValue
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
        .listOfN(20000, alu_gen)
        .sample
        .get

      val ref_seq = input_seq
        .map({ case (op_a, op_b, op_type, op_width) =>
          val res = alu_calculate_ref(op_a, op_b, op_type, op_width)
          long2UInt64(res)
        })
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

        // 随机生成的输入数据不能带 input 属性，不然在 match 的时候会出错
        // 以后带有 input 模块，需要使用的时候再生成，例如 AluIn
        val (seq1, seq2) =
          input_seq
            .map({ case (op_a, op_b, op_type, is_rv32) =>
              (new AluIn).Lit(
                _.a -> op_a,
                _.b -> op_b,
                _.trans_id -> 0.U,
                _.op -> op_type,
                _.is_rv32 -> is_rv32
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
          val res = (shift_op1.litValue.toLong << shift_op2.litValue.toInt)
          long2UInt64(res)
        })

      input_sll64_seq.zipWithIndex.foreach({
        case ((shift_op1, shift_op2), idx) =>
          dut.io.sll_req.poke(true.B)
          dut.io.srl_req.poke(false.B)
          dut.io.sra_req.poke(false.B)
          dut.io.shift32_req.poke(false.B)
          dut.io.shift_in.poke(shift_op1)
          dut.io.shift_count.poke(shift_op2)
          dut.io.shift_out.expect(expect_sll64_seq(idx))
          println(
            "sll64: shift_op1:%x, shift_op2:%x, dut_out:%x,ref_out:%x".format(
              shift_op1.litValue.toLong,
              shift_op2.litValue.toLong,
              dut.io.shift_out.peek().litValue.toLong,
              expect_sll64_seq(idx).litValue
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
          val res =
            (shift_op1.litValue.toInt << (shift_op2.litValue.toInt & 0x1f))
          int2UInt64(res)
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
          dut.io.shift_out.expect(expect_seq(idx))
          println(
            "sll64: shift_op1:%x, shift_op2:%x, dut_out:%x,ref_out:%x".format(
              shift_op1.litValue.toLong,
              shift_op2.litValue.toLong,
              dut.io.shift_out.peek().litValue.toLong,
              expect_seq(idx).litValue
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
