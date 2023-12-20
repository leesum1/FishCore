package leesum
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import leesum.mmu_sv39.{TLBEntry, TLB_L1}
import org.scalatest.freespec.AnyFreeSpec
class TLBTest extends AnyFreeSpec with ChiselScalatestTester {
  private def init_port(dut: TLB_L1) = {
    dut.io.tlb_update.setSourceClock(dut.clock)
    dut.io.tlb_flush.setSourceClock(dut.clock)
    dut.clock.step(5)

  }
  def gen_tlb_entry(
      va: UInt,
      asid: UInt,
      pte: UInt,
      pg_size: SV39PageSize.Type
  ) = {
    new TLBEntry().Lit(
      _.valid -> true.B,
      _.va -> va,
      _.asid -> asid,
      _.pte -> pte,
      _.pg_size -> pg_size
    )
  }

  "TLB_lookupTest" in {
    test(new TLB_L1(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        init_port(dut)

        // -------------------
        // prepare test data
        // -------------------
        // the last 12 bits of va is 0
        val tlb1 = gen_tlb_entry(0x1000.U, 0.U, 0x10000.U, SV39PageSize.SIZE4K)
        val tlb2 = gen_tlb_entry(0x2000.U, 0.U, 0x20000.U, SV39PageSize.SIZE4K)
        val tlb3 = gen_tlb_entry(0x3000.U, 0.U, 0x30000.U, SV39PageSize.SIZE4K)
        val tlb4 =
          gen_tlb_entry(
            0x200000.U,
            0.U,
            0x40000.U,
            SV39PageSize.SIZE2M
          ) // the last 21 bits of va is 0
        dut.io.tlb_update.enqueueSeq(Seq(tlb1, tlb2, tlb3, tlb4))
        dut.clock.step(5)

        // -------------------
        // lookup
        // -------------------
        // should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1000.U)
          dut.io.asid.poke(0.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x10000.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }
        // should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1123.U)
          dut.io.asid.poke(0.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x10000.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }
        // should miss
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1123.U)
          dut.io.asid.poke(2.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(false.B)
        }

        dut.clock.step(10)
      }
  }

  "TLB_lookup_g_bit_test" in {
    test(new TLB_L1(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        init_port(dut)

        // -------------------
        // prepare test data
        // -------------------
        // the last 12 bits of va is 0
        // g bit is 1
        val tlb1 = gen_tlb_entry(0x1000.U, 20.U, 0x10020.U, SV39PageSize.SIZE4K)
        // g bit is 0
        val tlb2 = gen_tlb_entry(0x2000.U, 0.U, 0x20000.U, SV39PageSize.SIZE4K)

        dut.io.tlb_update.enqueueSeq(Seq(tlb1, tlb2))
        dut.clock.step(5)

        // -------------------
        // lookup
        // -------------------
        // should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1123.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x10020.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }
        // should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1333.U)
          dut.io.asid.poke(20.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x10020.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }
        // should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x2123.U)
          dut.io.asid.poke(0.U) // asid is 0
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x20000.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }
        // should miss
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x2223.U)
          dut.io.asid.poke(2.U) // asid is 2
          dut.clock.step(1)
          dut.io.tlb_hit.expect(false.B)
        }

        dut.clock.step(5)
      }
  }

  // TODO: test tlb replace
  "TLB_replace_test" in {
    test(new TLB_L1(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        init_port(dut)

        // -------------------
        // prepare test data
        // -------------------
        // the last 12 bits of va is 0
        val tlb1 = gen_tlb_entry(0x1000.U, 0.U, 0x10000.U, SV39PageSize.SIZE4K)
        val tlb2 = gen_tlb_entry(0x2000.U, 0.U, 0x20000.U, SV39PageSize.SIZE4K)
        val tlb3 = gen_tlb_entry(0x3000.U, 0.U, 0x30000.U, SV39PageSize.SIZE4K)
        val tlb4 =
          gen_tlb_entry(
            0x200000.U,
            0.U,
            0x40000.U,
            SV39PageSize.SIZE2M
          ) // the last 21 bits of va is 0

        val tlb5 = gen_tlb_entry(0x4000.U, 0.U, 0x50000.U, SV39PageSize.SIZE4K)
        val tlb6 = gen_tlb_entry(0x5000.U, 0.U, 0x60000.U, SV39PageSize.SIZE4K)

        dut.io.tlb_update.enqueueSeq(Seq(tlb1, tlb2, tlb3, tlb4, tlb5, tlb6))
        dut.clock.step(5)

      }
  }

  "TLB_flush_all_test" in {
    test(new TLB_L1(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        init_port(dut)

        // -------------------
        // prepare test data
        // -------------------
        // the last 12 bits of va is 0
        val tlb1 = gen_tlb_entry(0x1000.U, 10.U, 0x10000.U, SV39PageSize.SIZE4K)
        val tlb2 = gen_tlb_entry(0x2000.U, 10.U, 0x20000.U, SV39PageSize.SIZE4K)
        val tlb3 = gen_tlb_entry(0x3000.U, 10.U, 0x30000.U, SV39PageSize.SIZE4K)
        val tlb4 =
          gen_tlb_entry(
            0x200000.U,
            20.U,
            0x40000.U,
            SV39PageSize.SIZE2M
          ) // the last 21 bits of va is 0

        dut.io.tlb_update.enqueueSeq(Seq(tlb1, tlb2, tlb3, tlb4))
        dut.clock.step(5)

        // -------------------
        // lookup
        // -------------------
        // should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1000.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x10000.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }

        // flush all tlb
        timescope {
          dut.io.tlb_flush.valid.poke(true.B)
          dut.io.tlb_flush.bits.va.poke(0x0.U)
          dut.io.tlb_flush.bits.asid.poke(0.U)
          dut.clock.step(1)
          dut.io.debug_tlb_count.expect(0.U)
        }

        // should miss
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1000.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(false.B)
        }
        dut.clock.step(5)
      }
  }

  "TLB_flush_va_test" in {
    test(new TLB_L1(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        init_port(dut)

        // -------------------
        // prepare test data
        // -------------------
        // the last 12 bits of va is 0
        // g bit is 1
        val tlb1 = gen_tlb_entry(0x1000.U, 10.U, 0x10020.U, SV39PageSize.SIZE4K)
        val tlb2 = gen_tlb_entry(0x2000.U, 10.U, 0x20000.U, SV39PageSize.SIZE4K)
        val tlb3 = gen_tlb_entry(0x3000.U, 10.U, 0x30000.U, SV39PageSize.SIZE4K)
        val tlb4 =
          gen_tlb_entry(
            0x200000.U,
            20.U,
            0x40000.U,
            SV39PageSize.SIZE2M
          ) // the last 21 bits of va is 0

        dut.io.tlb_update.enqueueSeq(Seq(tlb1, tlb2, tlb3, tlb4))
        dut.clock.step(5)

        // -------------------
        // lookup
        // -------------------
        // tlb1 should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1020.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x10020.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }
        // tlb2 should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x2020.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x20000.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }

        // flush tlb1(g bit is 1)
        timescope {
          dut.io.tlb_flush.valid.poke(true.B)
          dut.io.tlb_flush.bits.va.poke(0x1234.U)
          dut.io.tlb_flush.bits.asid.poke(0.U)
          dut.clock.step(1)
          dut.io.debug_tlb_count.expect(3.U)
        }
        // flush tlb2(g bit is 0)
        timescope {
          dut.io.tlb_flush.valid.poke(true.B)
          dut.io.tlb_flush.bits.va.poke(0x2234.U)
          dut.io.tlb_flush.bits.asid.poke(0.U)
          dut.clock.step(1)
          dut.io.debug_tlb_count.expect(2.U)
        }

        // tlb1 should miss
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1300.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(false.B)

        }
        // tlb2 should miss
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x2900.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(false.B)
        }
        dut.clock.step(5)
      }
  }

  "TLB_flush_asid_test" in {
    test(new TLB_L1(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        init_port(dut)

        // -------------------
        // prepare test data
        // -------------------
        // the last 12 bits of va is 0
        // g bit is 1
        val tlb1 = gen_tlb_entry(0x1000.U, 10.U, 0x10020.U, SV39PageSize.SIZE4K)
        val tlb2 = gen_tlb_entry(0x2000.U, 10.U, 0x20000.U, SV39PageSize.SIZE4K)
        val tlb3 = gen_tlb_entry(0x3000.U, 20.U, 0x30000.U, SV39PageSize.SIZE4K)
        val tlb4 =
          gen_tlb_entry(
            0x200000.U,
            20.U,
            0x40000.U,
            SV39PageSize.SIZE2M
          ) // the last 21 bits of va is 0

        dut.io.tlb_update.enqueueSeq(Seq(tlb1, tlb2, tlb3, tlb4))
        dut.clock.step(5)

        // -------------------
        // lookup
        // -------------------
        // tlb1 should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1020.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x10020.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }
        // tlb2 should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x2020.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x20000.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }

        // flush asid 10
        // tlb1 asid is 10, g bit is 1
        // tlb2 asid is 10, g bit is 0
        // tlb2 should be flushed
        timescope {
          dut.io.tlb_flush.valid.poke(true.B)
          dut.io.tlb_flush.bits.va.poke(0x0.U)
          dut.io.tlb_flush.bits.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.debug_tlb_count.expect(3.U)
        }

        // tlb1 should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1320.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x10020.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }
        // tlb2 should miss
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x2900.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(false.B)
        }
        dut.clock.step(5)
      }
  }
  "TLB_flush_asid_va_test" in {
    test(new TLB_L1(4))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        init_port(dut)

        // -------------------
        // prepare test data
        // -------------------
        // the last 12 bits of va is 0
        // g bit is 1
        val tlb1 = gen_tlb_entry(0x1000.U, 10.U, 0x10020.U, SV39PageSize.SIZE4K)
        val tlb2 = gen_tlb_entry(0x2000.U, 10.U, 0x20000.U, SV39PageSize.SIZE4K)
        val tlb3 = gen_tlb_entry(0x3000.U, 20.U, 0x30000.U, SV39PageSize.SIZE4K)
        val tlb4 =
          gen_tlb_entry(
            0x200000.U,
            20.U,
            0x40000.U,
            SV39PageSize.SIZE2M
          ) // the last 21 bits of va is 0

        dut.io.tlb_update.enqueueSeq(Seq(tlb1, tlb2, tlb3, tlb4))
        dut.clock.step(5)

        // -------------------
        // lookup
        // -------------------
        // tlb1 should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1020.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x10020.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }
        // tlb2 should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x2020.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x20000.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }

        // flush asid 10 va 0x2234
        // tlb1 asid is 10, va is 0x1000, g bit is 1
        // tlb2 asid is 10, va is 0x2000 ,g bit is 0
        // tlb2 should be flushed
        timescope {
          dut.io.tlb_flush.valid.poke(true.B)
          dut.io.tlb_flush.bits.va.poke(0x2234.U)
          dut.io.tlb_flush.bits.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.debug_tlb_count.expect(3.U)
        }

        // flush asid 10 va 0x1234
        // tlb1 asid is 10, va is 0x1000, g bit is 1
        // tlb2 asid is 10, va is 0x2000 ,g bit is 0
        // tlb1 should not be flushed
        timescope {
          dut.io.tlb_flush.valid.poke(true.B)
          dut.io.tlb_flush.bits.va.poke(0x1234.U)
          dut.io.tlb_flush.bits.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.debug_tlb_count.expect(3.U)
        }

        // tlb1 should hit
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x1320.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(true.B)
          dut.io.tlb_hit_pte.expect(0x10020.U)
          dut.io.tlb_hit_pg_size.expect(SV39PageSize.SIZE4K)
        }
        // tlb2 should miss
        timescope {
          dut.io.va.valid.poke(true.B)
          dut.io.va.bits.poke(0x2900.U)
          dut.io.asid.poke(10.U)
          dut.clock.step(1)
          dut.io.tlb_hit.expect(false.B)
        }
        dut.clock.step(5)
      }
  }
}
