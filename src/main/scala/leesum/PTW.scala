package leesum
import chisel3._
import chisel3.util.{Cat, Decoupled, Enum, MuxLookup, is, switch}
import chiseltest.ChiselScalatestTester
import chiseltest.formal.{
  BoundedCheck,
  CVC4EngineAnnotation,
  Formal,
  past,
  stable
}
import org.scalatest.flatspec.AnyFlatSpec

object SV39PageSize extends ChiselEnum {
  val SIZE4K = Value(0.U)
  val SIZE2M = Value(1.U)
  val SIZE1G = Value(2.U)
}

class PTWReq extends Bundle {
  val vaddr = UInt(64.W)
  val req_type = TLBReqType()
  val info = new MMUEffectiveInfo()
}

class PTWResp extends Bundle {
  val pte = UInt(64.W)
  val va = UInt(64.W)
  val exception = new ExceptionEntry()
  val pg_size = SV39PageSize()
  val asid = UInt(16.W)
}

class SV39VA(vaddr: UInt) {
  val offset = vaddr(11, 0)
  val ppn0 = vaddr(12 + 9, 12)
  val ppn1 = vaddr(12 + 9 * 2, 12 + 9)
  val ppn2 = vaddr(12 + 9 * 3, 12 + 9 * 2)

  def get_ppn(idx: UInt) = {
    assert(idx < 3.U)
    val ppn = WireInit(0.U(9.W))
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

class PTW(formal: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val flush = Input(Bool())
    val ptw_req = Flipped(Decoupled(new PTWReq))
    val ptw_resp = Decoupled(new PTWResp)
    val dcache_load_req = Decoupled(new LoadDcacheReq)
    val dcache_load_resp = Flipped(Decoupled(new LoadDcacheResp))
  })

  // When SUM=0, S-mode memory accesses to pages that are
  // accessible by U-mode (U=1 in Figure 4.18) will fault.
  // When SUM=1, these accesses are permitted.
  def is_mstatus_sum_check_pass(
      pte: SV39PTE,
      ptw_req: PTWReq
  ): Bool = {
    val sum_pass = Wire(Bool())
    when(ptw_req.info.mmu_privilege =/= Privilegelevel.S.U) {
      sum_pass := true.B
    }.otherwise {
      sum_pass := ptw_req.info.mstatus_field.sum || !pte.u
    }
    sum_pass
  }

  // 5. A leaf PTE has been found. Determine if the requested memory access is allowed by the
  // pte.r, pte.w, pte.x, and pte.u bits, given the current privilege mode and the value of the
  // SUM and MXR fields of the mstatus register. If not, stop and raise a page-fault exception
  // corresponding to the original access type.
  def is_pte_permission_check_pass(pte: SV39PTE, ptw_req: PTWReq) = {
    val pte_permission_check_pass = WireInit(false.B)
    val sum_check_pass = is_mstatus_sum_check_pass(pte, ptw_req)
    when(ptw_req.req_type === TLBReqType.Fetch) {
      pte_permission_check_pass := pte.x
    }.elsewhen(TLBReqType.need_store(ptw_req.req_type)) {
      // STORE , AMO , sc
      val store_check_pass = pte.w
      pte_permission_check_pass := store_check_pass & sum_check_pass

    }.elsewhen(TLBReqType.need_load(ptw_req.req_type)) {
      // When MXR=0, only loads from pages marked readable (R=1 in Figure 4.18) will succeed.
      // When MXR=1, loads from pages marked either readable or executable (R=1 or X=1) will succeed.
      // MXR has no effect when page-based virtual memory is not in effect.
      val load_check_pass = pte.r || pte.x & ptw_req.info.mstatus_field.mxr
      pte_permission_check_pass := load_check_pass & sum_check_pass
    }.otherwise {
      assert(false.B, "unknown req_type")
    }
    pte_permission_check_pass
  }

  val sIdle :: sSendDcacheReq :: sWaitDcacheResp :: sSendPTWResp :: sFlushWaitDcacheRespHs :: Nil =
    Enum(
      5
    )
  val ptw_req_buf = RegInit(0.U.asTypeOf(new PTWReq))
  val ptw_resp_buf = RegInit(0.U.asTypeOf(new PTWResp))

  val state = RegInit(sIdle)

  val ptw_info_level = 3.U
  val ptw_info_page_size = 4096
  val ptw_info_pte_size = 8
  val ptw_info_i = RegInit(ptw_info_level - 1.U)
  val ptw_info_a = RegInit(0.U(64.W)) // Let a be satp.ppn × PAGE SIZE
  val ptw_info_asid = RegInit(0.U(12.W))

  io.ptw_req.nodeq()
  io.ptw_resp.noenq()
  io.dcache_load_req.noenq()
  io.dcache_load_resp.nodeq()

  private def send_page_fault(ptw_req: PTWReq): Unit = {
    state := sSendPTWResp
    ptw_resp_buf.exception.valid := true.B
    ptw_resp_buf.exception.cause := ExceptionCause
      .get_page_fault_cause(
        ptw_req.req_type
      )
    ptw_resp_buf.exception.tval := ptw_req.vaddr
  }

  // 6. If i > 0 and pte.ppn[i − 1 : 0] ̸= 0, this is a misaligned superpage; stop and raise a page-fault
  // exception corresponding to the original access type.
  private def is_misaligned_superpage(i: UInt, pte: SV39PTE): Bool = {
    val is_misaligned = MuxLookup(
      i,
      false.B
    ) {
      Seq(
        1.U -> (pte.ppn0 =/= 0.U),
        2.U -> (Cat(pte.ppn1, pte.ppn0) =/= 0.U)
      )
    }
    is_misaligned
  }

  private def get_page_size(i: UInt): SV39PageSize.Type = {
    MuxLookup(
      i,
      SV39PageSize.SIZE4K
    ) {
      Seq(
        0.U -> SV39PageSize.SIZE4K,
        1.U -> SV39PageSize.SIZE2M,
        2.U -> SV39PageSize.SIZE1G
      )
    }
  }

  switch(state) {
    is(sIdle) {
      // accept new ptw_req
      // 1. Let a be satp.ppn × PAGESIZE, and let i = LEVELS − 1. (For Sv32, PAGESIZE=2^12 and
      // LEVELS=2.) The satp register must be active, i.e., the effective privilege mode must be
      // S-mode or U-mode.
      io.ptw_req.ready := true.B && !io.flush
      when(io.ptw_req.fire) {
        ptw_req_buf := io.ptw_req.bits
        ptw_resp_buf := 0.U.asTypeOf(new PTWResp)
        ptw_info_i := ptw_info_level - 1.U
        ptw_info_a := Cat(io.ptw_req.bits.info.satp_field.ppn, 0.U(12.W))
        ptw_info_asid := io.ptw_req.bits.info.satp_field.asid
        state := sSendDcacheReq
      }
    }
    is(sSendDcacheReq) {
      // 2. Let pte be the value of the PTE at address a+va.vpn[i]×PTESIZE. (For Sv32, PTESIZE=4.)
      // If accessing pte violates a PMA or PMP check, raise an access-fault exception corresponding
      // to the original access type.
      val va = new SV39VA(ptw_req_buf.vaddr)
      io.dcache_load_req.valid := true.B
      io.dcache_load_req.bits.size := DcacheConst.SIZE8
      io.dcache_load_req.bits.is_mmio := false.B
      // TODO: use concat
      io.dcache_load_req.bits.paddr := ptw_info_a + va.get_ppn(
        ptw_info_i
      ) * ptw_info_pte_size.U

      when(io.dcache_load_req.fire && !io.flush) {
        state := sWaitDcacheResp
      }.elsewhen(io.dcache_load_req.fire && io.flush) {
        state := sFlushWaitDcacheRespHs
      }.elsewhen(!io.dcache_load_req.fire && io.flush) {
        state := sIdle
      }
    }
    is(sWaitDcacheResp) {
      io.dcache_load_resp.ready := true.B
      when(io.dcache_load_resp.fire && !io.flush) {
        val pte = new SV39PTE(io.dcache_load_resp.bits.data)
        // 3. If pte.v = 0, or if pte.r = 0 and pte.w = 1, or if any bits or encodings that are reserved for
        // future standard use are set within pte, stop and raise a page-fault exception corresponding
        // to the original access type.
        val pte_page_fault_cond1 = !pte.v || (!pte.r && pte.w)

        when(pte_page_fault_cond1) {
          send_page_fault(ptw_req_buf)
        }.otherwise {
          // 4. Otherwise, the PTE is valid. If pte.r = 1 or pte.x = 1, go to step 5. Otherwise, this PTE is a
          // pointer to the next level of the page table. Let i = i − 1. If i < 0, stop and raise a page-fault
          // exception corresponding to the original access type. Otherwise, let a = pte.ppn × PAGESIZE
          // and go to step 2.
          val pte_is_leaf = pte.r || pte.x
          val pte_page_fault_cond2 = !pte_is_leaf && ptw_info_i === 0.U

          when(pte_is_leaf) {
            // 5. A leaf PTE has been found. Determine if the requested memory access is allowed by the
            // pte.r, pte.w, pte.x, and pte.u bits, given the current privilege mode and the value of the
            // SUM and MXR fields of the mstatus register. If not, stop and raise a page-fault exception
            // corresponding to the original access type.
            val pte_permission_pass =
              is_pte_permission_check_pass(pte, ptw_req_buf)
            when(!pte_permission_pass) {
              send_page_fault(ptw_req_buf)
            }.otherwise {
              // 6. If i > 0 and pte.ppn[i − 1 : 0] ̸= 0, this is a misaligned superpage; stop and raise a page-fault
              // exception corresponding to the original access type.
              val superpage_misaligned =
                is_misaligned_superpage(ptw_info_i, pte)
              // 7. If pte.a = 0, or if the original memory access is a store and pte.d = 0, either raise a page-fault
              // exception corresponding to the original access type
              val pte_page_fault_cond7 =
                !pte.a || (!pte.d && TLBReqType.need_store(
                  ptw_req_buf.req_type
                ))

              when(superpage_misaligned | pte_page_fault_cond7) {
                send_page_fault(ptw_req_buf)
              }.otherwise {
                // 8. The translation is successful. The translated physical address is given as follows:
                //      + pa.pgoff = va.pgoff.
                //      + If i > 0, then this is a superpage translation and pa.ppn[i − 1 : 0] = va.vpn[i − 1 : 0].
                //      + pa.ppn[LEVELS − 1 : i] = pte.ppn[LEVELS − 1 : i].
                ptw_resp_buf.va := ptw_req_buf.vaddr
                ptw_resp_buf.asid := ptw_info_asid
                ptw_resp_buf.pg_size := get_page_size(ptw_info_i)
                ptw_resp_buf.pte := pte.raw
                ptw_resp_buf.exception.valid := false.B
                state := sSendPTWResp
              }
            }
          }.elsewhen(pte_page_fault_cond2) {
            send_page_fault(ptw_req_buf)
          }.otherwise {
            ptw_info_i := ptw_info_i - 1.U
            ptw_info_a := Cat(pte.ppn_all, 0.U(12.W))
            state := sSendDcacheReq
          }
        }
      }.elsewhen(io.dcache_load_resp.fire && io.flush) {
        state := sIdle
      }.elsewhen(!io.dcache_load_resp.fire && io.flush) {
        state := sFlushWaitDcacheRespHs
      }
    }
    is(sSendPTWResp) {
      io.ptw_resp.valid := true.B
      io.ptw_resp.bits := ptw_resp_buf
      when(io.ptw_resp.fire || io.flush) {
        state := sIdle
      }
    }

    is(sFlushWaitDcacheRespHs) {
      io.dcache_load_resp.ready := true.B
      when(io.dcache_load_resp.fire) {
        state := sIdle
      }
    }
  }

  // --------------------------
  // formal
  // --------------------------

  // past(io.flush, 1)
  when(RegNext(io.flush)) {
    assert(
      state === sIdle || state === sFlushWaitDcacheRespHs || state === sSendPTWResp
    )
    assert(io.ptw_resp.valid === false.B)
    assert(io.dcache_load_req.valid === false.B)
  }

  when(io.flush) {
    assert(io.ptw_req.ready === false.B)
  }

  if (formal) {
    when(
      FormalUtils.StreamShouldStable(io.dcache_load_req) && !past(io.flush)
    ) {
      assert(io.dcache_load_req.valid)
      assert(stable(io.dcache_load_req.bits))
    }
    when(FormalUtils.StreamShouldStable(io.dcache_load_resp)) {
      assume(io.dcache_load_resp.valid)
      assume(stable(io.dcache_load_resp.bits))
    }
  }
}

class PTWFormal extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "PTW" should "pass with assumption" in {
    verify(
      new PTW(formal = true),
      Seq(BoundedCheck(10), CVC4EngineAnnotation)
    )
  }
}

object gen_PTW_verilog extends App {
  GenVerilogHelper(new PTW())
}
