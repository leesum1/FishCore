package leesum.mmu_sv39
import chisel3._
import chisel3.util.{Cat, Mux1H, MuxLookup}
import leesum.{Privilegelevel, TLBReqType}
object SV39PKG {

  // When SUM=0, S-mode memory accesses to pages that are
  // accessible by U-mode (U=1 in Figure 4.18) will fault.
  // When SUM=1, these accesses are permitted.
  private def leaf_pte_sum_permission_check(
      pte: SV39PTE,
      privilege_mode: UInt,
      sum_bit: Bool
  ): Bool = {
    require(privilege_mode.getWidth == 2)
    val sum_pass = Wire(Bool())
    when(privilege_mode =/= Privilegelevel.S.U) {
      sum_pass := true.B
    }.otherwise {
      sum_pass := sum_bit || !pte.u
    }
    sum_pass
  }

  // 7. If pte.a = 0, or if the original memory access is a store and pte.d = 0, either raise a page-fault
  // exception corresponding to the original access type
  private def leaf_pte_ad_check7(
      pte: SV39PTE,
      req_type: TLBReqType.Type
  ): Bool = {
    val raise_pg =
      !pte.a || (!pte.d && TLBReqType.need_store(
        req_type
      ))
    !raise_pg
  }

  // 5. A leaf PTE has been found. Determine if the requested memory access is allowed by the
  // pte.r, pte.w, pte.x, and pte.u bits, given the current privilege mode and the value of the
  // SUM and MXR fields of the mstatus register. If not, stop and raise a page-fault exception
  // corresponding to the original access type.
  private def leaf_pte_permission_check5(
      pte: SV39PTE,
      req_type: TLBReqType.Type,
      privilege_mode: UInt,
      mxr_bit: Bool,
      sum_bit: Bool
  ) = {
    require(privilege_mode.getWidth == 2)

    val pte_permission_check_pass = WireInit(false.B)
    val sum_check_pass =
      leaf_pte_sum_permission_check(pte, privilege_mode, sum_bit)
    when(req_type === TLBReqType.Fetch) {
      pte_permission_check_pass := pte.x
    }.elsewhen(TLBReqType.need_store(req_type)) {
      // STORE , AMO , sc
      val store_check_pass = pte.w
      pte_permission_check_pass := store_check_pass & sum_check_pass

    }.elsewhen(TLBReqType.need_load(req_type)) {
      // When MXR=0, only loads from pages marked readable (R=1 in Figure 4.18) will succeed.
      // When MXR=1, loads from pages marked either readable or executable (R=1 or X=1) will succeed.
      // MXR has no effect when page-based virtual memory is not in effect.
      val load_check_pass = pte.r || pte.x & mxr_bit
      pte_permission_check_pass := load_check_pass & sum_check_pass
    }.otherwise {
      assert(false.B, "unknown req_type")
    }
    pte_permission_check_pass
  }

  def leaf_pte_permission_check_all(
      pte: SV39PTE,
      req_type: TLBReqType.Type,
      privilege_mode: UInt,
      mxr_bit: Bool,
      sum_bit: Bool
  ): Bool = {
    val pte_pass = leaf_pte_permission_check5(
      pte = pte,
      req_type = req_type,
      privilege_mode = privilege_mode,
      mxr_bit = mxr_bit,
      sum_bit = sum_bit
    )

    val a_d_pass = leaf_pte_ad_check7(
      pte = pte,
      req_type = req_type
    )

    dontTouch(pte_pass)
    dontTouch(a_d_pass)

    pte_pass && a_d_pass
  }

  // 6. If i > 0 and pte.ppn[i − 1 : 0] ̸= 0, this is a misaligned superpage; stop and raise a page-fault
  // exception corresponding to the original access type.
  def misaligned_superpage_check(i: UInt, pte: SV39PTE): Bool = {
    val is_misaligned_spp = MuxLookup(
      i,
      false.B
    ) {
      Seq(
        1.U -> (pte.ppn0 =/= 0.U),
        2.U -> (Cat(pte.ppn1, pte.ppn0) =/= 0.U)
      )
    }
    !is_misaligned_spp
  }

  def get_page_start_addr(addr: UInt, pg_size: SV39PageSize.Type): UInt = {
    val sv39_va = new SV39VA(addr)
    MuxLookup(
      pg_size,
      0.U
    ) {
      Seq(
        SV39PageSize.SIZE1G -> Cat(sv39_va.ppn2, 0.U(30.W)),
        SV39PageSize.SIZE2M -> Cat(sv39_va.ppn2, sv39_va.ppn1, 0.U(21.W)),
        SV39PageSize.SIZE4K -> Cat(
          sv39_va.ppn2,
          sv39_va.ppn1,
          sv39_va.ppn0,
          0.U(12.W)
        )
      )
    }
  }

  def trans_va2pa(pte: UInt, va: UInt, pg_size: SV39PageSize.Type): UInt = {
    val pte_tmp = new SV39PTE(pte)
    val paddr_4k = Cat(pte_tmp.ppn_all, va(11, 0))
    val paddr_2m = Cat(pte_tmp.ppn2, pte_tmp.ppn1, va(20, 0))
    val paddr_1g = Cat(pte_tmp.ppn2, va(29, 0))

    val paddr = Mux1H(
      Seq(
        pg_size === SV39PageSize.SIZE1G,
        pg_size === SV39PageSize.SIZE2M,
        pg_size === SV39PageSize.SIZE4K
      ),
      Seq(
        paddr_1g,
        paddr_2m,
        paddr_4k
      )
    )

    paddr
  }

}

class SV39VA(vaddr: UInt) {
  val offset = vaddr(11, 0)
  val ppn0 = vaddr(11 + 9, 12)
  val ppn1 = vaddr(11 + 9 * 2, 12 + 9)
  val ppn2 = vaddr(11 + 9 * 3, 12 + 9 * 2)

  def get_ppn(idx: UInt) = {
    assert(idx < 3.U)
    val ppn = WireInit(0.U(9.W))

    require(ppn2.getWidth == 9)
    require(ppn1.getWidth == 9)
    require(ppn0.getWidth == 9)

    ppn := MuxLookup(
      idx,
      0.U
    ) {
      Seq(
        0.U -> ppn0,
        1.U -> ppn1,
        2.U -> ppn2
      )
    }
    ppn
  }
}

class SV39PTE(pte: UInt) {
  val v = pte(0)
  val r = pte(1)
  val w = pte(2)
  val x = pte(3)
  val u = pte(4)
  val g = pte(5)
  val a = pte(6)
  val d = pte(7)
  val rsw = pte(9, 8)
  val ppn0 = pte(18, 10)
  val ppn1 = pte(27, 19)
  val ppn2 = pte(53, 28)
  val reserved = pte(60, 54)
  val pbmt = pte(62, 61)
  val n = pte(63)

  def ppn_all = Cat(ppn2, ppn1, ppn0)

  def raw = pte
}

object SV39PageSize extends ChiselEnum {
  val SIZE4K = Value(0.U)
  val SIZE2M = Value(1.U)
  val SIZE1G = Value(2.U)
}
