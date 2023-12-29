package leesum.mmu_sv39
import chisel3._
import chisel3.util.{
  Cat,
  Mux1H,
  MuxLookup,
  OHToUInt,
  PopCount,
  PriorityEncoder,
  Valid,
  isPow2,
  log2Ceil
}
import leesum.Utils.{HoldRegister, LFSRRand, PLRU}
import leesum.{CheckOverlap, GenVerilogHelper}

class TLBEntry extends Bundle {
  val valid = Bool()
  val pte = UInt(64.W)
  val va = UInt(64.W)
  val pg_size = SV39PageSize()
  def pg_1g = pg_size === SV39PageSize.SIZE1G
  def pg_2m = pg_size === SV39PageSize.SIZE2M
  def pg_4k = pg_size === SV39PageSize.SIZE4K

  val asid = UInt(16.W)
}

class SfenceVMABundle extends Bundle {
  val asid = UInt(16.W)
  val va = UInt(64.W)
}

class TLB_L1(num: Int) extends Module {

  require(num > 1 && isPow2(num), "TLB_L1 num should be power of 2")

  val io = IO(new Bundle {
    // tlb lookup
    val va = Input(Valid(UInt(64.W)))
    val asid = Input(UInt(16.W))
    // tlb hit info
    val tlb_hit = Output(Bool())
    val tlb_hit_pg_size = Output(SV39PageSize())
    val tlb_hit_pte = Output(UInt(64.W))
    // tlb update
    val tlb_update = Input(Valid(new TLBEntry))
    // tlb flush
    val tlb_flush = Input(Valid(new SfenceVMABundle))

    // debug port
    val debug_tlb_count = Output(UInt(log2Ceil(num + 1).W))
  })

  val tlb_content = RegInit(VecInit(Seq.fill(num)(0.U.asTypeOf(new TLBEntry))))

  /** This function matches a virtual address (va) with a TLB entry. It checks
    * if the TLB entry is valid and if the global (g) bit is set or the ASID
    * matches the input ASID. Depending on the page size, it checks if the page
    * number of the lookup address matches the page number of the TLB entry. If
    * a match is found, it sets the TLB hit signal to true.
    *
    * @param va
    *   The virtual address to be matched.
    * @param tlb_entry
    *   The TLB entry to be matched with.
    * @return
    *   A tuple containing the TLB hit signal, the page size, and the page table
    *   entry.
    */
  def tlb_match(va: UInt, tlb_entry: TLBEntry) = {
    require(va.getWidth == 64)
    val lookup_va = new SV39VA(va)
    val tlb_va = new SV39VA(tlb_entry.va)

    val tlb_pte = new SV39PTE(tlb_entry.pte)

    val pg_size = tlb_entry.pg_size
    val tlb_hit = WireDefault(false.B)

    when(
      tlb_entry.valid && (tlb_pte.g || tlb_entry.asid === io.asid)
    ) {
      tlb_hit := MuxLookup(
        tlb_entry.pg_size,
        false.B
      ) {
        Seq(
          SV39PageSize.SIZE1G -> (lookup_va.ppn2 === tlb_va.ppn2),
          SV39PageSize.SIZE2M -> (lookup_va.ppn2 === tlb_va.ppn2 && lookup_va.ppn1 === tlb_va.ppn1),
          SV39PageSize.SIZE4K -> (lookup_va.ppn2 === tlb_va.ppn2 && lookup_va.ppn1 === tlb_va.ppn1 && lookup_va.ppn0 === tlb_va.ppn0)
        )
      }
    }
    (tlb_hit, pg_size, tlb_entry.pte)
  }
  // --------------------
  // tlb lookup
  // --------------------
  val tlb_match_info = tlb_content.map(tlb_match(io.va.bits, _))
  val tlb_match_hits = VecInit(tlb_match_info.map(_._1))
  val tlb_match_pg_sizes = VecInit(tlb_match_info.map(_._2))
  val tlb_match_ptes = VecInit(tlb_match_info.map(_._3))

  val tlb_hit = tlb_match_hits.reduce(_ || _) && io.va.valid
  val tlb_hit_pte = Mux1H(tlb_match_hits, tlb_match_ptes)
  val tlb_hit_pg_size = Mux1H(tlb_match_hits, tlb_match_pg_sizes)
  val tlb_hit_idx = OHToUInt(tlb_match_hits)

  io.tlb_hit := HoldRegister(io.va.valid, RegNext(tlb_hit), 1)
  io.tlb_hit_pte := HoldRegister(io.va.valid, RegNext(tlb_hit_pte), 1)
  io.tlb_hit_pg_size := HoldRegister(io.va.valid, RegNext(tlb_hit_pg_size), 1)

  // --------------------
  // tlb update
  // --------------------

  val tlb_has_empty = tlb_content.map(!_.valid).reduce(_ || _)
  val tlb_first_empty_idx = PriorityEncoder(tlb_content.map(!_.valid))

  // Pseudo-LRU
  val tlb_plru = Module(new PLRU(num))
  tlb_plru.io.update_valid := io.tlb_update.valid || tlb_hit
  tlb_plru.io.update_data := Mux(
    tlb_hit,
    tlb_hit_idx,
    Mux(tlb_has_empty, tlb_first_empty_idx, tlb_plru.io.out)
  )

//  // replace algorithm TODO: Pseudo-LRU
//  val tlb_rand_update_idx = LFSRRand(num)

  val tlb_update_idx = Mux(
    tlb_has_empty,
    tlb_first_empty_idx,
    tlb_plru.io.out
  )

  // update
  when(io.tlb_update.valid) {
    assert(io.tlb_update.bits.valid, "TLB update should be valid")
    tlb_content(tlb_update_idx) := io.tlb_update.bits
  }

  // flush will override tlb update
  for (i <- 0 until num) {
    when(io.tlb_flush.valid) {
      val flush_va_is0 = io.tlb_flush.bits.va === 0.U
      val flush_asid_is0 = io.tlb_flush.bits.asid === 0.U
      val flush_va = new SV39VA(io.tlb_flush.bits.va)
      val tlb_va = new SV39VA(tlb_content(i).va)
      val tlb_pte = new SV39PTE(tlb_content(i).pte)

      val va_eq = MuxLookup(
        tlb_content(i).pg_size,
        false.B
      ) {
        Seq(
          SV39PageSize.SIZE1G -> (flush_va.ppn2 === tlb_va.ppn2),
          SV39PageSize.SIZE2M -> (Cat(flush_va.ppn2, flush_va.ppn1) === Cat(
            tlb_va.ppn2,
            tlb_va.ppn1
          )),
          SV39PageSize.SIZE4K -> (Cat(
            flush_va.ppn2,
            flush_va.ppn1,
            flush_va.ppn0
          ) === Cat(tlb_va.ppn2, tlb_va.ppn1, tlb_va.ppn0))
        )
      }
      val asid_eq = tlb_content(i).asid === io.tlb_flush.bits.asid

      when(flush_asid_is0 && flush_va_is0) {
        // flush all
        tlb_content(i).valid := false.B
      }.elsewhen(flush_va_is0 && asid_eq && !tlb_pte.g) {
        // flush asid with g bit is 0
        tlb_content(i).valid := false.B
      }.elsewhen(flush_asid_is0 && va_eq) {
        // flush va
        tlb_content(i).valid := false.B
      }.elsewhen(
        va_eq && asid_eq && !tlb_pte.g && !flush_va_is0 && !flush_asid_is0
      ) {
        // flush va and asid with g bit is 0
        tlb_content(i).valid := false.B
      }
    }
  }

  // --------------------
  // debug
  // --------------------

  io.debug_tlb_count := PopCount(tlb_content.map(_.valid))

  // --------------------
  // assert
  // --------------------
  assert(
    PopCount(tlb_match_hits) <= 1.U,
    "TLB hit should be one-hot encoded"
  )

  // check overlap about tlb
  for (i <- 0 until num) {
    val other_content = tlb_content.zipWithIndex.filter(_._2 != i).map(_._1)
    val other_range = other_content.map(c => {
      (
        c.valid, // valid
        c.va, // start
        c.va + Mux(
          c.pg_1g,
          0x40000000.U,
          Mux(c.pg_2m, 0x200000.U, 0x1000.U)
        ) // end
      )
    })

    other_range.foreach(r => {
      when(r._1 && tlb_content(i).valid) {
        assert(
          !CheckOverlap(
            tlb_content(i).va,
            r._2,
            r._3
          ),
          "TLB entry %d should not overlap".format(i)
        )
      }
    })
  }
  // check page size
  for (i <- 0 until num) {
    when(tlb_content(i).valid) {
      val va_aligned = MuxLookup(
        tlb_content(i).pg_size,
        false.B
      ) {
        Seq(
          SV39PageSize.SIZE1G -> (tlb_content(i).va(11 + 18, 0) === 0.U),
          SV39PageSize.SIZE2M -> (tlb_content(i).va(11 + 9, 0) === 0.U),
          SV39PageSize.SIZE4K -> (tlb_content(i).va(11, 0) === 0.U)
        )
      }

      assert(
        va_aligned,
        "TLB entry va %d should be aligned".format(i)
      )
    }
  }
}

object gen_TLB_L1_verilog extends App {
  GenVerilogHelper(new TLB_L1(8))

}
