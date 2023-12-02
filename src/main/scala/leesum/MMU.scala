package leesum

import chisel3._
import chisel3.util.{Cat, Decoupled, DecoupledIO, Enum, Valid, is, switch}
import chiseltest.ChiselScalatestTester
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec

class TLBEntry extends Bundle {
  val pte = UInt(64.W)
  val va = UInt(64.W)
  val pg_size = SV39PageSize()
  val asid = UInt(16.W)
}

class TLBL1(num: Int) extends Module {
  val io = IO(new Bundle {
    val va = Input(Valid(UInt(64.W)))
    val tlb_hit = Output(Bool())
    val tlb_entry = Output(new TLBEntry)
  })
  // TODO: not implemented
  io.tlb_hit := false.B
  io.tlb_entry := DontCare
  io.tlb_entry.va := io.va.bits
}

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
  })

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
        addr >= Long2UInt64(start) && addr <= Long2UInt64(end)
      }
      .reduce(_ || _)

    in_range
  }

  def check_mmio(paddr: UInt): Bool = {

    val is_mmio = WireInit(false.B)
    addr_map
      .foreach { case (start, end, ismmio) =>
        when(
          paddr >= Long2UInt64(start) && paddr < Long2UInt64(
            end
          )
        ) {
          is_mmio := ismmio.B
        }
      }
    is_mmio
  }

  def get_paddr(pte: UInt, va: UInt, pg_size: SV39PageSize.Type): UInt = {
    val pte_tmp = new SV39PTE(pte)
    val paddr = Wire(UInt(64.W))
    val paddr_4k = Cat(pte_tmp.ppn_all, va(11, 0))
    val paddr_2m = Cat(pte_tmp.ppn2, pte_tmp.ppn1, va(20, 0))
    val paddr_1g = Cat(pte_tmp.ppn2, va(29, 0))

    when(pg_size === SV39PageSize.SIZE1G) {
      paddr := paddr_1g
    }.elsewhen(pg_size === SV39PageSize.SIZE2M) {
      paddr := paddr_2m
    }.otherwise {
      paddr := paddr_4k
    }
    paddr
  }

  def gen_no_mmu_resp(req_buf: TLBReq): TLBResp = {
    val no_mmu_resp = Wire(new TLBResp)
    no_mmu_resp.req_type := req_buf.req_type
    no_mmu_resp.paddr := req_buf.vaddr
    no_mmu_resp.size := req_buf.size
    no_mmu_resp.is_mmio := check_mmio(req_buf.vaddr)

    when(!addr_in_range(req_buf.vaddr)) {
      // 2. out of range, TODO: not PMA
      no_mmu_resp.exception.valid := true.B
      no_mmu_resp.exception.tval := req_buf.vaddr
      no_mmu_resp.exception.cause := ExceptionCause.get_access_cause(
        req_buf.req_type
      )
    }.elsewhen(
      !CheckAligned(
        req_buf.vaddr,
        req_buf.size
      ) && req_buf.req_type =/= TLBReqType.Fetch
    ) {
      // 3. not aligned
      no_mmu_resp.exception.valid := true.B
      no_mmu_resp.exception.tval := req_buf.vaddr
      no_mmu_resp.exception.cause := ExceptionCause.get_misaligned_cause(
        req_buf.req_type
      )
    }.otherwise {
      no_mmu_resp.exception.valid := false.B
      no_mmu_resp.exception.tval := 0.U
      no_mmu_resp.exception.cause := ExceptionCause.unknown
    }

    no_mmu_resp
  }

  val itlb_req = io.fetch_req.valid
  val dtlb_req = io.lsu_req.valid

  // TODO: hit should keep last result
  val itlb_hit = Wire(Bool())
  val dtlb_hit = Wire(Bool())

  val itlb_l1 = Module(new TLBL1(4))
  val dtlb_l1 = Module(new TLBL1(4))

  val ptw_arb = Module(new ReqRespArbiter(2, new PTWReq, new PTWResp))
  val ptw = Module(new PTW(formal = formal))

  ptw.io.dcache_load_req <> io.dcache_load_req
  ptw.io.dcache_load_resp <> io.dcache_load_resp
  ptw.io.ptw_req <> ptw_arb.io.req_arb
  ptw.io.ptw_resp <> ptw_arb.io.resp_arb

  ptw.io.flush := io.flush

  itlb_l1.io.va.valid := itlb_req
  itlb_l1.io.va.bits := io.fetch_req.bits.vaddr
  itlb_hit := itlb_l1.io.tlb_hit
  dtlb_l1.io.va.valid := dtlb_req
  dtlb_l1.io.va.bits := io.lsu_req.bits.vaddr
  dtlb_hit := dtlb_l1.io.tlb_hit

  val sIdle :: sTLBLookup :: sWaitPTWResp :: sSendResp :: Nil =
    Enum(4)
  val itlb_state = RegInit(sIdle)
  val dtlb_state = RegInit(sIdle)

  val fetch_req_buf = RegInit(0.U.asTypeOf(new TLBReq))
  val lsu_req_buf = RegInit(0.U.asTypeOf(new TLBReq))
  val fetch_resp_buf = RegInit(0.U.asTypeOf(new TLBResp))
  val lsu_resp_buf = RegInit(0.U.asTypeOf(new TLBResp))
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
      flush: Bool
  ) = {
    mmu_resp_io.valid := true.B
    mmu_resp_io.bits := resp_buf
    when(mmu_resp_io.fire && !flush) {
      // back to back
      rev_fetch_req()
    }.elsewhen(flush) {
      // cancel resp
      assert(!mmu_resp_io.fire)
      state := sIdle
    }
  }

  def lsu_send_mmu_resp(
      mmu_resp_io: DecoupledIO[TLBResp],
      resp_buf: TLBResp,
      state: UInt,
      flush: Bool
  ) = {
    mmu_resp_io.valid := true.B
    mmu_resp_io.bits := resp_buf
    when(mmu_resp_io.fire && !flush) {
      // back to back
      rev_lsu_req()
    }.elsewhen(flush) {
      // cancel resp
      assert(!mmu_resp_io.fire)
      state := sIdle
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
        when(itlb_hit) {
          // itlb hit TODO: not implemented
//          assert(false, "not implemented")
          itlb_state := sIdle
        }.otherwise {
          // itlb miss, send ptw req
          send_ptw_req(
            ptw_req = fetch_ptw_req,
            req_buf = fetch_req_buf,
            fetch_effective_info_buf,
            state = itlb_state,
            flush = io.flush
          )
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
        val paddr = get_paddr(
          pte = tlb_tmp.pte,
          va = tlb_tmp.va,
          pg_size = tlb_tmp.pg_size
        )
        fetch_resp_buf.paddr := paddr
        fetch_resp_buf.req_type := fetch_req_buf.req_type
        fetch_resp_buf.size := fetch_req_buf.size
        fetch_resp_buf.is_mmio := check_mmio(paddr)

        // exception
        when(fetch_ptw_resp.bits.exception.valid) {
          // 1. exception from ptw
          fetch_resp_buf.exception := fetch_ptw_resp.bits.exception
        }.elsewhen(!addr_in_range(paddr)) {
          // 2. out of range, TODO: not PMA
          fetch_resp_buf.exception.valid := true.B
          fetch_resp_buf.exception.tval := fetch_req_buf.vaddr
          fetch_resp_buf.exception.cause := ExceptionCause.fetch_access
        }.otherwise {
          fetch_resp_buf.exception.valid := false.B
          fetch_resp_buf.exception.tval := 0.U
          fetch_resp_buf.exception.cause := ExceptionCause.unknown
        }

        itlb_state := sSendResp
      }.elsewhen(io.flush) {
        assert(!fetch_ptw_resp.fire)
        // cancel ptw resp
        itlb_state := sIdle
      }
    }
    is(sSendResp) {
      fetch_send_mmu_resp(
        mmu_resp_io = io.fetch_resp,
        resp_buf = fetch_resp_buf,
        state = itlb_state,
        flush = io.flush
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
        when(dtlb_hit) {
          // dtlb hit TODO: not implemented
          //          assert(false, "not implemented")
          dtlb_state := sIdle
        }.otherwise {
          // dtlb miss
          send_ptw_req(
            ptw_req = lsu_ptw_req,
            req_buf = lsu_req_buf,
            lsu_effective_info_buf,
            state = dtlb_state,
            flush = io.flush
          )
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

        val paddr = get_paddr(
          pte = tlb_tmp.pte,
          va = tlb_tmp.va,
          pg_size = tlb_tmp.pg_size
        )
        lsu_resp_buf.paddr := paddr

        lsu_resp_buf.req_type := lsu_req_buf.req_type
        lsu_resp_buf.size := lsu_req_buf.size
        lsu_resp_buf.is_mmio := check_mmio(paddr)

        when(lsu_ptw_resp.bits.exception.valid) {
          // 1. exception from ptw
          lsu_resp_buf.exception := lsu_ptw_resp.bits.exception
        }.elsewhen(!addr_in_range(paddr)) {
          // 2. out of range, TODO: not PMA or page check
          lsu_resp_buf.exception.valid := true.B
          lsu_resp_buf.exception.tval := lsu_req_buf.vaddr
          lsu_resp_buf.exception.cause := ExceptionCause.get_access_cause(
            lsu_req_buf.req_type
          )
        }.elsewhen(!CheckAligned(paddr, lsu_req_buf.size)) {
          // 3. not aligned
          lsu_resp_buf.exception.valid := true.B
          lsu_resp_buf.exception.tval := lsu_req_buf.vaddr
          lsu_resp_buf.exception.cause := ExceptionCause.get_misaligned_cause(
            lsu_req_buf.req_type
          )
        }.otherwise {
          lsu_resp_buf.exception.valid := false.B
          lsu_resp_buf.exception.tval := 0.U
          lsu_resp_buf.exception.cause := ExceptionCause.unknown
        }

        dtlb_state := sSendResp
      }.elsewhen(io.flush) {
        assert(!lsu_ptw_resp.fire)
        // cancel ptw resp
        dtlb_state := sIdle
      }
    }
    is(sSendResp) {
      lsu_send_mmu_resp(
        mmu_resp_io = io.lsu_resp,
        resp_buf = lsu_resp_buf,
        state = dtlb_state,
        flush = io.flush
      )
    }
  }

  // -------------------
  // formal
  // -------------------

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
