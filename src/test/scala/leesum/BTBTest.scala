package leesum

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.log2Ceil
import chiseltest._
import leesum.bpu.{BTBEntry, BTBNway, BTBPCField}
import leesum.mmu_sv39.{SV39PageSize, TLBEntry, TLB_L1}
import org.scalatest.freespec.AnyFreeSpec

class BTBTest extends AnyFreeSpec with ChiselScalatestTester {
  private def init_port(dut: TLB_L1) = {
    dut.io.tlb_update.setSourceClock(dut.clock)
    dut.io.tlb_flush.setSourceClock(dut.clock)
    dut.clock.step(5)

  }

//  class BTBEntry extends Bundle {
//    val target_pc = UInt(39.W)
//    val offset = UInt(log2Ceil(3).W)
//    val bp_type = BpType()
//  }

  def gen_blb_entry(
      va: UInt,
      target_pc: UInt,
      nums: Int,
      is_rvc: Boolean = false,
      is_branch: Boolean = true
  ) = {
    val vpc_field = new BTBPCField(va, nums)
    require(vpc_field.offset.getWidth == 3)

    // TODO: more bp type
    val bp_type = if (is_branch) BpType.Branch else BpType.None

    val btb_entry = new BTBEntry().Lit(
      _.offset -> vpc_field.offset,
      _.target_pc -> target_pc,
      _.bp_type -> bp_type,
      _.is_rvc -> is_rvc.B
    )

    (vpc_field.tag, vpc_field.index, btb_entry)
  }

  val index0_seq = Seq(
    (0x1000.U(39.W), 0x10001000.U(39.W)),
    (0x2002.U(39.W), 0x10002000.U(39.W)),
    (0x3004.U(39.W), 0x10003000.U(39.W)),
    (0x4006.U(39.W), 0x10004000.U(39.W))
  )

  val index0_replace = (0x8000.U(39.W), 0x20001000.U(39.W))

  val index1_seq = Seq(
    (0x1008.U(39.W), 0x10001008.U(39.W)),
    (0x200a.U(39.W), 0x10002008.U(39.W)),
    (0x300c.U(39.W), 0x10003008.U(39.W)),
    (0x400e.U(39.W), 0x10004008.U(39.W))
  )
  val index1_replace = (0x9008.U(39.W), 0x30001000.U(39.W))

  "BTB_lookupTest" in {
    test(new BTBNway(4, 8))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // -------------------
        // prepare test data
        // -------------------

        // full index0
        index0_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) = gen_blb_entry(va, target_pc, 8)

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }
        // full index1
        index1_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) = gen_blb_entry(va, target_pc, 8)

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }

        // -------------------
        // lookup
        // -------------------

        for (((va, target_pc), idx) <- index0_seq.zipWithIndex) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc.expect(target_pc)
            dut.io.target_pc_hit.expect(true.B)
            dut.io.hit_way.expect(idx.U)
          }
        }

        for (((va, target_pc), idx) <- index1_seq.zipWithIndex) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc.expect(target_pc)
            dut.io.target_pc_hit.expect(true.B)
            dut.io.hit_way.expect(idx.U)
          }
        }

        dut.clock.step(10)
      }
  }

  "BTB_FLushTest" in {
    test(new BTBNway(4, 8))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // -------------------
        // prepare test data
        // -------------------

        // full index0
        index0_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) = gen_blb_entry(va, target_pc, 8)

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }
        // full index1
        index1_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) = gen_blb_entry(va, target_pc, 8)

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }

        // -------------------
        // clear
        // -------------------

        timescope {
          dut.io.clear_en.poke(true.B)
          dut.clock.step(1)
        }

        for ((va, target_pc) <- index0_seq) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc_hit.expect(false.B)
          }
        }

        for ((va, target_pc) <- index1_seq) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc_hit.expect(false.B)
          }
        }

        dut.clock.step(10)
      }
  }

  "BTB_replaceTest" in {
    test(new BTBNway(4, 8))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        // -------------------
        // prepare test data
        // -------------------

        // full index0
        index0_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) = gen_blb_entry(va, target_pc, 8)

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }
        // full index1
        index1_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) = gen_blb_entry(va, target_pc, 8)

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }

        // -------------------
        // lookup
        // -------------------
        for ((va, target_pc) <- index0_seq) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc.expect(target_pc)
            dut.io.target_pc_hit.expect(true.B)
          }
        }

        for ((va, target_pc) <- index1_seq) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc.expect(target_pc)
            dut.io.target_pc_hit.expect(true.B)
          }
        }

        // -------------------
        // replace
        // -------------------
        for ((va, target_pc) <- Seq(index0_replace, index1_replace)) {
          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(gen_blb_entry(va, target_pc, 8)._3)
            dut.io.refill_way_sel.poke(0.U)
            dut.clock.step(1)
          }
        }

        // after replace lookup, should hit
        for ((va, target_pc) <- Seq(index0_replace, index1_replace)) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc.expect(target_pc)
            dut.io.target_pc_hit.expect(true.B)
          }
        }
        // after replace lookup, should miss
        for ((va, target_pc) <- Seq(index0_seq.head, index1_seq.head)) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc_hit.expect(false.B)
          }
        }

        dut.clock.step(10)
      }
  }

  "BTB_PCOffsetTest" in {
    test(new BTBNway(4, 8))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // -------------------
        // prepare test data
        // -------------------

        // full index0
        index0_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) = gen_blb_entry(va, target_pc, 8)

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }
        // full index1
        index1_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) = gen_blb_entry(va, target_pc, 8)

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }

        // ----------------------
        // lookup, test pc offset
        // ----------------------

//        val index0_seq = Seq(
//          (0x1000.U(39.W), 0x10001000.U(39.W)),
//          (0x2002.U(39.W), 0x10002000.U(39.W)),
//          (0x3004.U(39.W), 0x10003000.U(39.W)),
//          (0x4006.U(39.W), 0x10004000.U(39.W))
//        )

        // should hit
        timescope {
          dut.io.lookup_en.poke(true.B)
          dut.io.lookup_pc.poke(0x1000.U(39.W))
          dut.clock.step(1)
          dut.io.target_pc.expect(0x10001000.U(39.W))
          dut.io.target_pc_hit.expect(true.B)
        }
        // should miss
        timescope {
          dut.io.lookup_en.poke(true.B)
          dut.io.lookup_pc.poke(0x1004.U(39.W))
          dut.clock.step(1)
          dut.io.target_pc_hit.expect(false.B)
        }

        // should hit
        for (i <- 0 to 4) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke((0x3000 + i).U(39.W))
            dut.clock.step(1)
            dut.io.target_pc.expect(0x10003000.U(39.W))
            dut.io.target_pc_hit.expect(true.B)
          }
        }
        // should miss
        for (i <- 5 to 7) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke((0x3000 + i).U(39.W))
            dut.clock.step(1)
            dut.io.target_pc_hit.expect(false.B)
          }
        }

        dut.clock.step(10)
      }
  }

  "BTB_NoneTypeTest" in {
    test(new BTBNway(4, 8))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
      ) { dut =>
        // -------------------
        // prepare test data
        // -------------------

        // full index0
        index0_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) = gen_blb_entry(va, target_pc, 8)

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }
        // full index1
        index1_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) = gen_blb_entry(va, target_pc, 8)

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }

        // -------------------
        // lookup
        // -------------------
        for ((va, target_pc) <- index0_seq) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc.expect(target_pc)
            dut.io.target_pc_hit.expect(true.B)
          }
        }

        for ((va, target_pc) <- index1_seq) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc.expect(target_pc)
            dut.io.target_pc_hit.expect(true.B)
          }
        }

        // -------------------
        // replace
        // -------------------

        // full index0 with NoneType
        index0_seq.zipWithIndex.foreach { case ((va, target_pc), i) =>
          val (tag, index, btb_entry) =
            gen_blb_entry(va, target_pc, 8, is_branch = false) // NoneType

          timescope {
            dut.io.refill_en.poke(true.B)
            dut.io.refill_pc.poke(va)
            dut.io.refill_data.poke(btb_entry)
            dut.io.refill_way_sel.poke(i.U)
            dut.clock.step(1)
          }
        }

        // after replace NoneType,lookup should miss
        for ((va, target_pc) <- Seq(index0_replace, index1_replace)) {
          timescope {
            dut.io.lookup_en.poke(true.B)
            dut.io.lookup_pc.poke(va)
            dut.clock.step(1)
            dut.io.target_pc_hit.expect(false.B)
          }
        }

        dut.clock.step(10)
      }
  }

}
