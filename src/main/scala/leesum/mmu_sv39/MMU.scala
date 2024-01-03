package leesum.mmu_sv39

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Enum, Valid, is, switch}
import chiseltest.ChiselScalatestTester
import chiseltest.formal._
import leesum.Cache.{LoadDcacheReq, LoadDcacheResp}
import leesum._
import leesum.moniter.PerfMonitorCounter
import org.scalatest.flatspec.AnyFlatSpec

class MMUEffectiveInfo extends Bundle {
  val mmu_privilege = UInt(2.W)
  val mstatus = UInt(64.W)
  val satp = UInt(64.W)

  val mstatus_field = new MstatusFiled(mstatus)
  val satp_field = new SatpFiled(satp)
}

class MMU(
    // ADDR_START, ADDR_END, mmio
    addr_map: Seq[(Long, Long, Boolean)] = Seq(
      (
        0,
        0xffffffffffffffffL,
        false
      ) // default addr map, 0x0 -> 0xffffffffffffffff, mmio = false
    ),
    formal: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    // fetch and lsu req
    val fetch_req = Flipped(Decoupled(new TLBReq))
    val fetch_resp = Decoupled(new TLBResp)
    val lsu_req = Flipped(Decoupled(new TLBReq))
    val lsu_resp = Decoupled(new TLBResp)
    // dcache interface
    val dcache_load_req = Decoupled(new LoadDcacheReq)
    val dcache_load_resp = Flipped(Decoupled(new LoadDcacheResp))
    // csr register interface
    val mstatus = Input(UInt(64.W))
    val satp = Input(UInt(64.W))
    val cur_privilege = Input(UInt(2.W))
    val flush = Input(Bool())
    // tlb flush
    val tlb_flush = Input(Valid(new SfenceVMABundle))

    // perf monitor
    val perf_itlb = Output(new PerfMonitorCounter)
    val perf_dtlb = Output(new PerfMonitorCounter)
  })
  val perf_itlb = RegInit(0.U.asTypeOf(new PerfMonitorCounter))
  val perf_dtlb = RegInit(0.U.asTypeOf(new PerfMonitorCounter))

  io.perf_itlb := perf_itlb
  io.perf_dtlb := perf_dtlb

  val mstatus_field = new MstatusFiled(io.mstatus)

  val fetch_effective_info_buf = RegInit(0.U.asTypeOf(new MMUEffectiveInfo))
  val lsu_effective_info_buf = RegInit(0.U.asTypeOf(new MMUEffectiveInfo))

  when(io.fetch_req.fire) {
    fetch_effective_info_buf.mmu_privilege := io.cur_privilege
    fetch_effective_info_buf.mstatus := io.mstatus
    fetch_effective_info_buf.satp := io.satp
  }
  when(io.lsu_req.fire) {
    // When MPRV=1, load and store memory addresses are translated and protected, and endianness is applied, as though
    // the current privilege mode were set to MPP. Instruction address-translation and protection are
    // unaffected by the setting of MPRV. MPRV is read-only 0 if U-mode is not supported.
    lsu_effective_info_buf.mmu_privilege := Mux(
      mstatus_field.mprv,
      mstatus_field.mpp,
      io.cur_privilege
    )
    lsu_effective_info_buf.mstatus := io.mstatus
    lsu_effective_info_buf.satp := io.satp
  }

  val fetch_mmu_en =
    (!fetch_effective_info_buf.satp_field.mode_is_bare) && !(fetch_effective_info_buf.mmu_privilege === Privilegelevel.M.U)
  val lsu_mmu_en =
    (!lsu_effective_info_buf.satp_field.mode_is_bare) && !(lsu_effective_info_buf.mmu_privilege === Privilegelevel.M.U)

  def addr_in_range(addr: UInt): Bool = {
    val in_range = addr_map
      .map { case (start, end, _) =>
        CheckOverlap(
          addr,
          Long2UInt64(start),
          Long2UInt64(end)
        )
      }
      .reduce(_ || _)

    in_range
  }

  def check_mmio(paddr: UInt): Bool = {
    val is_mmio = addr_map
      .filter(_._3) // only check mmio range
      .map { case (start, end, _) =>
        CheckOverlap(
          paddr,
          Long2UInt64(start),
          Long2UInt64(end)
        )
      }
      .reduceOption(_ || _)
      .getOrElse(false.B)
    is_mmio
  }

  def gen_access_misaligned_exception(
      paddr: UInt,
      vaddr: UInt,
      req_size: UInt,
      req_type: TLBReqType.Type,
      enable_misaligned_check: Boolean
  ): ExceptionEntry = {
    val exception = Wire(new ExceptionEntry)

    when(!addr_in_range(paddr)) {
      // 2. out of range, TODO: not PMA
      exception.valid := true.B
      exception.tval := vaddr
      exception.cause := ExceptionCause.get_access_cause(
        req_type
      )
    }.elsewhen(
      !CheckAligned(
        paddr,
        req_size
      ) && req_type =/= TLBReqType.Fetch && enable_misaligned_check.B
    ) {
      // 3. not aligned
      exception.valid := true.B
      exception.tval := vaddr
      exception.cause := ExceptionCause.get_misaligned_cause(
        req_type
      )
    }.otherwise {
      exception.valid := false.B
      exception.tval := 0.U
      exception.cause := ExceptionCause.unknown
    }

    exception
  }

  def gen_no_mmu_resp(req_buf: TLBReq): TLBResp = {
    val no_mmu_resp = Wire(new TLBResp)
    no_mmu_resp.req_type := req_buf.req_type
    no_mmu_resp.paddr := req_buf.vaddr
    no_mmu_resp.size := req_buf.size
    no_mmu_resp.is_mmio := check_mmio(req_buf.vaddr)
    no_mmu_resp.exception := gen_access_misaligned_exception(
      paddr = req_buf.vaddr, // vaddr = paddr
      vaddr = req_buf.vaddr,
      req_size = req_buf.size,
      req_type = req_buf.req_type,
      enable_misaligned_check = true
    )

    no_mmu_resp
  }

  val asid_val = new SatpFiled(io.satp).asid

  val itlb_req = io.fetch_req.fire
  val itlb_req_va = io.fetch_req.bits.vaddr
  val itlb_req_asid = asid_val
  val dtlb_req = io.lsu_req.fire
  val dtlb_req_va = io.lsu_req.bits.vaddr
  val dtlb_req_asid = asid_val

  val itlb_l1 = Module(new TLB_L1(8))
  val dtlb_l1 = Module(new TLB_L1(8))

  itlb_l1.io.va.valid := itlb_req
  itlb_l1.io.va.bits := itlb_req_va
  itlb_l1.io.asid := itlb_req_asid
  dtlb_l1.io.va.valid := dtlb_req
  dtlb_l1.io.va.bits := dtlb_req_va
  dtlb_l1.io.asid := dtlb_req_asid

  itlb_l1.io.tlb_update.valid := false.B
  itlb_l1.io.tlb_update.bits := DontCare
  dtlb_l1.io.tlb_update.valid := false.B
  dtlb_l1.io.tlb_update.bits := DontCare
  itlb_l1.io.tlb_flush := io.tlb_flush
  dtlb_l1.io.tlb_flush := io.tlb_flush

  val ptw_arb = Module(new ReqRespArbiter(2, new PTWReq, new PTWResp))
  val ptw = Module(new PTW(formal = formal))

  ptw.io.dcache_load_req <> io.dcache_load_req
  ptw.io.dcache_load_resp <> io.dcache_load_resp
  ptw.io.ptw_req <> ptw_arb.io.req_arb
  ptw.io.ptw_resp <> ptw_arb.io.resp_arb

  ptw.io.flush := io.flush

  val sIdle :: sTLBLookup :: sWaitPTWResp :: sSendResp :: Nil =
    Enum(4)
  val itlb_state = RegInit(sIdle)
  val dtlb_state = RegInit(sIdle)

  val fetch_req_buf = RegInit(0.U.asTypeOf(new TLBReq))
  val lsu_req_buf = RegInit(0.U.asTypeOf(new TLBReq))
  val fetch_resp_buf = RegInit(0.U.asTypeOf(new TLBResp))
  val lsu_resp_buf = RegInit(0.U.asTypeOf(new TLBResp))
  val fetch_ptw_resp_buf = RegInit(0.U.asTypeOf(new PTWResp))
  val lsu_ptw_resp_buf = RegInit(0.U.asTypeOf(new PTWResp))

  val fetch_ptw_req = Wire(Decoupled(new PTWReq))
  val fetch_ptw_resp = Wire(Flipped(Decoupled(new PTWResp)))
  val lsu_ptw_req = Wire(Decoupled(new PTWReq))
  val lsu_ptw_resp = Wire(Flipped(Decoupled(new PTWResp)))

  ptw_arb.io.flush := io.flush
  ptw_arb.io.req_vec(0) <> fetch_ptw_req
  ptw_arb.io.resp_vec(0) <> fetch_ptw_resp

  ptw_arb.io.req_vec(1) <> lsu_ptw_req
  ptw_arb.io.resp_vec(1) <> lsu_ptw_resp

  lsu_ptw_req.noenq()
  lsu_ptw_resp.nodeq()
  fetch_ptw_req.noenq()
  fetch_ptw_resp.nodeq()
  io.lsu_req.nodeq()
  io.fetch_req.nodeq()
  io.lsu_resp.noenq()
  io.fetch_resp.noenq()

  def send_ptw_req(
      ptw_req: DecoupledIO[PTWReq],
      req_buf: TLBReq,
      info: MMUEffectiveInfo,
      state: UInt,
      flush: Bool
  ) = {
    // tlb miss, send ptw req
    ptw_req.valid := true.B
    ptw_req.bits.req_type := req_buf.req_type
    ptw_req.bits.vaddr := req_buf.vaddr
    ptw_req.bits.info := info
    when(ptw_req.fire && !flush) {
      state := sWaitPTWResp
    }.elsewhen(flush) {
      // cancel ptw req
      assert(!ptw_req.fire)
      state := sIdle
    }
  }

  def rev_fetch_req() = {
    io.fetch_req.ready := true.B && !io.flush
    when(io.fetch_req.fire) {
      // send itlb req at the same cycle
      fetch_req_buf := io.fetch_req.bits
      itlb_state := sTLBLookup
    }.otherwise {
      itlb_state := sIdle
    }
  }

  def fetch_send_mmu_resp(
      mmu_resp_io: DecoupledIO[TLBResp],
      resp_buf: TLBResp,
      state: UInt,
      flush: Bool,
      rev_new_req: Boolean = true
  ) = {
    mmu_resp_io.valid := true.B
    mmu_resp_io.bits := resp_buf
    when(mmu_resp_io.fire && !flush) {
      // back to back
      if (rev_new_req) {
        rev_fetch_req()
      } else {
        state := sIdle
      }
    }.elsewhen(flush) {
      // cancel resp
      assert(!mmu_resp_io.fire)
      state := sIdle
    }.otherwise {
      state := sIdle
      assert(false.B, "unreachable")
    }
  }

  def lsu_send_mmu_resp(
      mmu_resp_io: DecoupledIO[TLBResp],
      resp_buf: TLBResp,
      state: UInt,
      flush: Bool,
      rev_new_req: Boolean = true
  ) = {
    mmu_resp_io.valid := true.B
    mmu_resp_io.bits := resp_buf
    when(mmu_resp_io.fire && !flush) {
      // back to back
      if (rev_new_req) {
        rev_lsu_req()
      } else {
        state := sIdle
      }
    }.elsewhen(flush) {
      // cancel resp
      assert(!mmu_resp_io.fire)
      state := sIdle
    }.otherwise {
      state := sIdle
      assert(false.B, "unreachable")
    }
  }

  // ----------------------------
  // ITLB
  // ----------------------------

  switch(itlb_state) {
    is(sIdle) {
      rev_fetch_req()
    }
    is(sTLBLookup) {
      // get tlb resp
      when(fetch_mmu_en) {
        assert(fetch_req_buf.req_type === TLBReqType.Fetch)
        // mmu enable
        when(itlb_l1.io.tlb_hit) {

          perf_itlb.inc_hit(1.U)

          // check permission
          val itlb_hit_pte = new SV39PTE(itlb_l1.io.tlb_hit_pte)
          val itlb_hit_permission_check_pass =
            SV39PKG.leaf_pte_permission_check_all(
              pte = itlb_hit_pte,
              req_type = TLBReqType.Fetch,
              privilege_mode = fetch_effective_info_buf.mmu_privilege,
              mxr_bit = fetch_effective_info_buf.mstatus_field.mxr,
              sum_bit = fetch_effective_info_buf.mstatus_field.sum
            )

          // itlb hit response
          val itlb_hit_resp = Wire(new TLBResp)
          itlb_hit_resp.req_type := TLBReqType.Fetch
          val itlb_hit_resp_paddr = SV39PKG.trans_va2pa(
            pte = itlb_l1.io.tlb_hit_pte,
            va = fetch_req_buf.vaddr,
            pg_size = itlb_l1.io.tlb_hit_pg_size
          )
          itlb_hit_resp.paddr := itlb_hit_resp_paddr
          itlb_hit_resp.size := fetch_req_buf.size
          itlb_hit_resp.is_mmio := check_mmio(itlb_hit_resp_paddr)

          // exception priority: page fault > access fault
          itlb_hit_resp.exception := Mux(
            itlb_hit_permission_check_pass,
            gen_access_misaligned_exception(
              paddr = itlb_hit_resp_paddr,
              vaddr = fetch_req_buf.vaddr,
              req_size = fetch_req_buf.size,
              req_type = TLBReqType.Fetch,
              enable_misaligned_check = false
            ),
            ExceptionEntry(
              true.B,
              fetch_req_buf.vaddr,
              ExceptionCause.fetch_access
            )
          )

          fetch_send_mmu_resp(
            mmu_resp_io = io.fetch_resp,
            resp_buf = itlb_hit_resp,
            state = itlb_state,
            flush = io.flush
          )
        }.otherwise {
          // itlb miss, send ptw req
          send_ptw_req(
            ptw_req = fetch_ptw_req,
            req_buf = fetch_req_buf,
            fetch_effective_info_buf,
            state = itlb_state,
            flush = io.flush
          )
          when(fetch_ptw_req.fire) {
            perf_itlb.inc_miss(1.U)
          }
        }
      }.otherwise {
        // mmu disable, send mmu resp
        val fetch_no_mmu_resp = gen_no_mmu_resp(fetch_req_buf)
        fetch_send_mmu_resp(
          mmu_resp_io = io.fetch_resp,
          resp_buf = fetch_no_mmu_resp,
          state = itlb_state,
          flush = io.flush
        )
      }
    }
    is(sWaitPTWResp) {
      fetch_ptw_resp.ready := true.B && !io.flush
      when(fetch_ptw_resp.fire && !io.flush) {
        // PTW resp
        val tlb_tmp = fetch_ptw_resp.bits
        fetch_ptw_resp_buf := fetch_ptw_resp.bits
        fetch_ptw_resp_buf.va := SV39PKG.get_page_start_addr(
          fetch_ptw_resp.bits.va,
          fetch_ptw_resp.bits.pg_size
        )

        val itb_ptw_resp_paddr = SV39PKG.trans_va2pa(
          pte = tlb_tmp.pte,
          va = tlb_tmp.va,
          pg_size = tlb_tmp.pg_size
        )
        fetch_resp_buf.paddr := itb_ptw_resp_paddr
        fetch_resp_buf.req_type := fetch_req_buf.req_type
        fetch_resp_buf.size := fetch_req_buf.size
        fetch_resp_buf.is_mmio := check_mmio(itb_ptw_resp_paddr)

        // exception priority: page fault > access fault
        fetch_resp_buf.exception := Mux(
          fetch_ptw_resp.bits.exception.valid,
          fetch_ptw_resp.bits.exception,
          gen_access_misaligned_exception(
            paddr = itb_ptw_resp_paddr, // use paddr
            vaddr = fetch_req_buf.vaddr,
            req_size = fetch_req_buf.size,
            req_type = TLBReqType.Fetch,
            enable_misaligned_check = false
          )
        )

        itlb_state := sSendResp
      }.elsewhen(io.flush) {
        assert(!fetch_ptw_resp.fire)
        // cancel ptw resp
        itlb_state := sIdle
      }
    }
    is(sSendResp) {
      // update dtlb
      itlb_l1.io.tlb_update.valid := !fetch_ptw_resp_buf.exception.valid
      itlb_l1.io.tlb_update.bits.valid := true.B
      itlb_l1.io.tlb_update.bits.va := fetch_ptw_resp_buf.va
      itlb_l1.io.tlb_update.bits.pte := fetch_ptw_resp_buf.pte
      itlb_l1.io.tlb_update.bits.pg_size := fetch_ptw_resp_buf.pg_size
      itlb_l1.io.tlb_update.bits.asid := fetch_ptw_resp_buf.asid

      fetch_send_mmu_resp(
        mmu_resp_io = io.fetch_resp,
        resp_buf = fetch_resp_buf,
        state = itlb_state,
        flush = io.flush,
        rev_new_req = false
      )
    }
  }

  // -------------------
  // DTLB
  // -------------------

  def rev_lsu_req() = {
    io.lsu_req.ready := true.B && !io.flush
    when(io.lsu_req.fire) {
      // send itlb req at the same cycle
      lsu_req_buf := io.lsu_req.bits
      dtlb_state := sTLBLookup
    }.otherwise {
      dtlb_state := sIdle
    }
  }

  switch(dtlb_state) {
    is(sIdle) {
      rev_lsu_req()
    }
    is(sTLBLookup) {
      // get tlb resp
      when(lsu_mmu_en) {
        assert(lsu_req_buf.req_type =/= TLBReqType.Fetch)
        // mmu enable
        when(dtlb_l1.io.tlb_hit) {
          // dtlb hit TODO: not implemented
          perf_dtlb.inc_hit(1.U)
          // check permission
          val dtlb_hit_pte = new SV39PTE(dtlb_l1.io.tlb_hit_pte)
          val dtlb_hit_permission_check_pass =
            SV39PKG.leaf_pte_permission_check_all(
              pte = dtlb_hit_pte,
              req_type = lsu_req_buf.req_type,
              privilege_mode = lsu_effective_info_buf.mmu_privilege,
              mxr_bit = lsu_effective_info_buf.mstatus_field.mxr,
              sum_bit = lsu_effective_info_buf.mstatus_field.sum
            )

          val dtlb_hit_resp = Wire(new TLBResp)
          dtlb_hit_resp.req_type := lsu_req_buf.req_type
          val dtlb_hit_resp_paddr = SV39PKG.trans_va2pa(
            pte = dtlb_l1.io.tlb_hit_pte,
            va = lsu_req_buf.vaddr,
            pg_size = dtlb_l1.io.tlb_hit_pg_size
          )
          dtlb_hit_resp.paddr := dtlb_hit_resp_paddr
          dtlb_hit_resp.size := lsu_req_buf.size
          dtlb_hit_resp.is_mmio := check_mmio(dtlb_hit_resp_paddr)

          // exception priority: page fault > access fault > misaligned fault
          dtlb_hit_resp.exception := Mux(
            dtlb_hit_permission_check_pass,
            gen_access_misaligned_exception(
              paddr = dtlb_hit_resp_paddr, // use paddr
              vaddr = lsu_req_buf.vaddr,
              req_size = lsu_req_buf.size,
              req_type = lsu_req_buf.req_type,
              enable_misaligned_check = true
            ),
            ExceptionEntry(
              true.B,
              lsu_req_buf.vaddr,
              ExceptionCause.get_page_fault_cause(
                lsu_req_buf.req_type
              )
            )
          )

          // send resp
          lsu_send_mmu_resp(
            mmu_resp_io = io.lsu_resp,
            resp_buf = dtlb_hit_resp,
            state = dtlb_state,
            flush = io.flush
          )
        }.otherwise {
          // dtlb miss
          send_ptw_req(
            ptw_req = lsu_ptw_req,
            req_buf = lsu_req_buf,
            lsu_effective_info_buf,
            state = dtlb_state,
            flush = io.flush
          )
          when(lsu_ptw_req.fire) {
            perf_dtlb.inc_miss(1.U)
          }
        }
      }.otherwise {
        // mmu disable
        val lsu_no_mmu_resp = gen_no_mmu_resp(lsu_req_buf)
        lsu_send_mmu_resp(
          mmu_resp_io = io.lsu_resp,
          resp_buf = lsu_no_mmu_resp,
          state = dtlb_state,
          flush = io.flush
        )
      }
    }
    is(sWaitPTWResp) {
      lsu_ptw_resp.ready := true.B && !io.flush
      when(lsu_ptw_resp.fire && !io.flush) {
        // PTW resp
        val tlb_tmp = lsu_ptw_resp.bits
        lsu_ptw_resp_buf := lsu_ptw_resp.bits
        lsu_ptw_resp_buf.va := SV39PKG.get_page_start_addr(
          lsu_ptw_resp.bits.va,
          lsu_ptw_resp.bits.pg_size
        )

        val dtlb_ptw_resp_paddr = SV39PKG.trans_va2pa(
          pte = tlb_tmp.pte,
          va = tlb_tmp.va,
          pg_size = tlb_tmp.pg_size
        )
        lsu_resp_buf.paddr := dtlb_ptw_resp_paddr

        lsu_resp_buf.req_type := lsu_req_buf.req_type
        lsu_resp_buf.size := lsu_req_buf.size
        lsu_resp_buf.is_mmio := check_mmio(dtlb_ptw_resp_paddr)

        // exception priority: page fault > access fault > misaligned fault
        lsu_resp_buf.exception := Mux(
          lsu_ptw_resp.bits.exception.valid,
          lsu_ptw_resp.bits.exception,
          gen_access_misaligned_exception(
            paddr = dtlb_ptw_resp_paddr, // use paddr
            vaddr = lsu_req_buf.vaddr,
            req_size = lsu_req_buf.size,
            req_type = lsu_req_buf.req_type,
            enable_misaligned_check = true
          )
        )

        dtlb_state := sSendResp
      }.elsewhen(io.flush) {
        assert(!lsu_ptw_resp.fire)
        // cancel ptw resp
        dtlb_state := sIdle
      }
    }
    is(sSendResp) {
      // update dtlb
      dtlb_l1.io.tlb_update.valid := !lsu_ptw_resp_buf.exception.valid
      dtlb_l1.io.tlb_update.bits.valid := true.B
      dtlb_l1.io.tlb_update.bits.va := lsu_ptw_resp_buf.va
      dtlb_l1.io.tlb_update.bits.pte := lsu_ptw_resp_buf.pte
      dtlb_l1.io.tlb_update.bits.pg_size := lsu_ptw_resp_buf.pg_size
      dtlb_l1.io.tlb_update.bits.asid := lsu_ptw_resp_buf.asid

      // send resp
      lsu_send_mmu_resp(
        mmu_resp_io = io.lsu_resp,
        resp_buf = lsu_resp_buf,
        state = dtlb_state,
        flush = io.flush,
        rev_new_req = false
      )
    }
  }

  // -------------------
  // formal
  // -------------------

  when(io.tlb_flush.valid) {
    assert(io.flush)
  }

  when(RegNext(io.flush)) {
    assert(itlb_state === sIdle)
    assert(dtlb_state === sIdle)
  }

  when(io.flush) {
    assume(!io.fetch_resp.ready)
    assume(!io.lsu_resp.ready)
  }

  when(io.flush) {
    assert(!io.fetch_req.fire)
    assert(!io.lsu_req.fire)
    assert(!io.fetch_resp.fire)
    assert(!io.lsu_resp.fire)
  }

  when(io.lsu_req.fire) {
    assume(io.lsu_req.bits.req_type =/= TLBReqType.Fetch)
  }
  when(io.fetch_req.fire) {
    assume(io.fetch_req.bits.req_type === TLBReqType.Fetch)
  }
  when(io.fetch_resp.fire) {
    assert(io.fetch_resp.bits.req_type === TLBReqType.Fetch)
  }
  when(io.lsu_resp.fire) {
    assert(io.lsu_resp.bits.req_type =/= TLBReqType.Fetch)
  }

  if (formal) {

    val f_flush = io.flush | past(io.flush)

    when(FormalUtils.StreamShouldStable(io.fetch_req) && !f_flush) {
      assume(io.fetch_req.valid)
      assume(stable(io.fetch_req.bits))
    }
    when(FormalUtils.StreamShouldStable(io.lsu_req) && !f_flush) {
      assume(io.lsu_req.valid)
      assume(stable(io.lsu_req.bits))
    }
    when(FormalUtils.StreamShouldStable(io.fetch_resp) && !f_flush) {
      assert(io.fetch_resp.valid)
      assert(stable(io.fetch_resp.bits))
    }
    when(FormalUtils.StreamShouldStable(io.lsu_resp) && !f_flush) {
      assert(io.lsu_resp.valid)
      assert(stable(io.lsu_resp.bits))
    }

    when(!stable(io.mstatus) || !stable(io.satp) || !stable(io.cur_privilege)) {
      assume(io.flush)
    }
  }
}

class MMUFormal extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "MMU" should "pass with assumption" in {
    verify(
      new MMU(formal = true),
      Seq(BoundedCheck(6), CVC4EngineAnnotation)
    )
  }
}

object gen_mmu_verilog extends App {
  GenVerilogHelper(new MMU)
}
