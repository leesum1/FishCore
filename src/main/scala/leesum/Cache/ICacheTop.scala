package leesum.Cache

import chisel3._
import chisel3.util.{Cat, Decoupled, Enum, is, switch}
import chiseltest.ChiselScalatestTester
import chiseltest.formal.{
  BoundedCheck,
  CVC4EngineAnnotation,
  Formal,
  past,
  stable
}
import leesum.axi4.{AXIDef, AXIMasterIO, StreamFork2}
import leesum._
import leesum.moniter.PerfMonitorCounter
import org.scalatest.flatspec.AnyFlatSpec

class ICacheReq extends Bundle {
  val va = UInt(64.W)
}
class ICacheResp extends Bundle {
  val payload = UInt(64.W)
  val exception = new ExceptionEntry()
}
class ICacheTop(formal: Boolean = false) extends Module {

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new ICacheReq))
    val resp = Decoupled(new ICacheResp)
    val flush = Input(Bool())
    // mmu port
    val mmu_req = Decoupled(new TLBReq)
    val mmu_resp = Flipped(Decoupled(new TLBResp))
    // memory port
    val mem_master = new AXIMasterIO(32, 64)

    // fencei
    val fencei = Input(Bool()) // when fencei is true, flush must be true

    // perf monitor
    val perf_icache = Output(new PerfMonitorCounter)
  })

  val icache_perf = RegInit(0.U.asTypeOf(new PerfMonitorCounter))
  val refill_buf = RegInit(0.U.asTypeOf(Vec(2, UInt(64.W))))
  val refill_buf_idx = RegInit(0.U(1.W))
  val icache_rdata_buf = RegInit(0.U(64.W))
  // icache should keep the last rdata until the next read
  val icache2way = Module(new ICache2way(4))
  icache2way.io.clear_en := io.fencei // fencei will clear the icache
  icache2way.io.addr := io.req.bits.va(38, 0)
  icache2way.io.lookup_en := io.req.fire
  icache2way.io.plookup_tag := 0.U
  icache2way.io.refill_en := false.B
  icache2way.io.refill_index := 0.U
  icache2way.io.refill_data := refill_buf.asUInt
  icache2way.io.refill_tag := 0.U
  val icache_hit = icache2way.io.cache_data_hit

  io.perf_icache := icache_perf

  io.mem_master.clear()
  io.mem_master.ar.bits.id := 0.U
  io.mem_master.ar.bits.lock := false.B
  io.mem_master.ar.bits.cache := 0.U
  io.mem_master.ar.bits.prot := 0.U
  io.mem_master.ar.bits.qos := 0.U
  io.mem_master.ar.bits.region := 0.U
  io.mem_master.ar.bits.user := 0.U

  io.mem_master.aw.bits.id := 0.U
  io.mem_master.aw.bits.lock := false.B
  io.mem_master.aw.bits.cache := 0.U
  io.mem_master.aw.bits.prot := 0.U
  io.mem_master.aw.bits.qos := 0.U
  io.mem_master.aw.bits.region := 0.U
  io.mem_master.aw.bits.user := 0.U

  io.req.nodeq()
  io.resp.noenq()
  io.mmu_req.noenq()
  io.mmu_resp.nodeq()

  val sIdle :: sWaitTLBResp :: sSendICacheResp :: sSendRefillReq :: sWaitRefillResp :: sFlushRefillReq :: sFlushRefillResp :: Nil =
    Enum(7)

  val state = RegInit(sIdle)
  val icache_req_buf = RegInit(0.U.asTypeOf(new ICacheReq))
  val mmu_resp_buf = RegInit(0.U.asTypeOf(new TLBResp))

  def rev_icache_req() = {
    val (icache_req_fork, mmu_req_fork) =
      StreamFork2(io.req, synchronous = true)

    icache_req_fork.ready := !io.flush
    mmu_req_fork.ready := io.mmu_req.ready && !io.flush
    // Send mmu request
    io.mmu_req.valid := mmu_req_fork.valid
    io.mmu_req.bits.req_type := TLBReqType.Fetch
    io.mmu_req.bits.vaddr := io.req.bits.va
    io.mmu_req.bits.size := DcacheConst.SIZE8

    when(io.req.fire) {
      icache_req_buf := io.req.bits
      state := sWaitTLBResp
      // TODO: send read req of icache tag and data, resp at the next cycle
//      icache_data := 1122334.U // set a random value
//      icache_tag := 5566778.U // set a random value
    }.otherwise {
      state := sIdle
    }
  }

  switch(state) {
    is(sIdle) {
      rev_icache_req()
    }
    is(sWaitTLBResp) {
      io.mmu_resp.ready := true.B && !io.flush

      when(io.mmu_resp.fire) {
        // wait for mmu response
        // at the same time, icache tag and data are ready, compare paddr with tag to check if hit
        assert(io.mmu_resp.bits.req_type === TLBReqType.Fetch)
        mmu_resp_buf := io.mmu_resp.bits
        icache2way.io.plookup_tag := io.mmu_resp.bits.paddr(38, 10) // 29 bits

        when(icache_hit) {
          // send response at the same cycle
          io.resp.valid := true.B
          io.resp.bits.payload := icache2way.io.cache_data
          io.resp.bits.exception := io.mmu_resp.bits.exception
          icache_perf.inc_hit(1.U)

          when(io.resp.fire) {
            // TODO: back by back
            rev_icache_req()
//            state := sIdle
          }.otherwise {
            // FetchStage can't accept resp at the same cycle
            state := sSendICacheResp
          }
        }.otherwise {
          icache_perf.inc_miss(1.U)
          // not hit, send refill request
          when(io.mmu_resp.bits.exception.valid) {
            state := sSendICacheResp
          }.otherwise {
            // TODO: send refill request at the same cycle?
            state := sSendRefillReq
          }
        }
      }.elsewhen(io.flush) {
        state := sIdle
      }
    }

    is(sSendRefillReq) {
      assert(mmu_resp_buf.req_type === TLBReqType.Fetch)
      assert(!mmu_resp_buf.exception.valid)

      io.mem_master.ar.valid := true.B
      io.mem_master.ar.bits.addr := Cat(
        mmu_resp_buf.paddr(63, 4),
        0.U(4.W)
      ) // aligned to 16 bytes
      io.mem_master.ar.bits.size := DcacheConst.SIZE8
      io.mem_master.ar.bits.burst := DcacheConst.BURST_INCR
      io.mem_master.ar.bits.len := 1.U
      io.mem_master.ar.bits.id := 0.U // TODO: not implemented now

      refill_buf_idx := 0.U

      when(io.mem_master.ar.fire && !io.flush) {
        state := sWaitRefillResp
      }.elsewhen(io.mem_master.ar.fire && io.flush) {
        state := sFlushRefillResp
      }.elsewhen(!io.mem_master.ar.fire && io.flush) {
        state := sFlushRefillReq
      }

    }
    is(sWaitRefillResp) {
      assert(mmu_resp_buf.req_type === TLBReqType.Fetch)

      io.mem_master.r.ready := true.B
      when(io.mem_master.r.fire && !io.flush) {
        // success, no flush
        refill_buf(refill_buf_idx) := io.mem_master.r.bits.data
        refill_buf_idx := refill_buf_idx + 1.U

        when(io.mem_master.r.bits.last) {
          state := sSendICacheResp

          // we can get the all refill data the same cycle
          val refill_data_tmp = VecInit(
            refill_buf
              .dropRight(1)
              .appended(io.mem_master.r.bits.data)
          )
          require(refill_data_tmp.length == refill_buf.length)

          // refill icache
          icache2way.io.refill_en := true.B
          icache2way.io.refill_tag := mmu_resp_buf.paddr(38, 10) // 29 bits
          icache2way.io.refill_index := mmu_resp_buf.paddr(9, 4) // 6 bits
          icache2way.io.refill_data := refill_data_tmp.asUInt

          // select the right data
          icache_rdata_buf := refill_data_tmp(mmu_resp_buf.paddr(3))
        }
      }.elsewhen(
        io.mem_master.r.fire && io.mem_master.r.bits.last && io.flush
      ) {
        // encounter flush, and is last, go sIdle
        state := sIdle
      }.elsewhen(io.flush) {
        // encounter flush, but not last, go sFlushRefillResp wait for last
        state := sFlushRefillResp
      }
    }

    is(sSendICacheResp) {
//      assert(
//        mmu_resp_buf.exception.valid,
//        "only send resp on icache_hit or page exception"
//      )

      io.resp.valid := true.B
      io.resp.bits.payload := icache_rdata_buf
      io.resp.bits.exception := mmu_resp_buf.exception
      when(io.resp.fire && !io.flush) {
        rev_icache_req()
      }.elsewhen(io.flush) {
        // cancel the response
        state := sIdle
      }
    }

    is(sFlushRefillReq) {
      assert(mmu_resp_buf.req_type === TLBReqType.Fetch)
      io.mem_master.ar.valid := true.B
      io.mem_master.ar.bits.addr := Cat(mmu_resp_buf.paddr(63, 3), 0.U(3.W))
      io.mem_master.ar.bits.size := DcacheConst.SIZE8
      io.mem_master.ar.bits.burst := 0.U
      io.mem_master.ar.bits.len := 0.U
      io.mem_master.ar.bits.id := 0.U // TODO: not implemented now
      when(io.mem_master.ar.fire) {
        state := sFlushRefillResp
      }
    }
    is(sFlushRefillResp) {
      assert(mmu_resp_buf.req_type === TLBReqType.Fetch)
      io.mem_master.r.ready := true.B
      when(io.mem_master.r.fire) {
//        assert(io.mem_master.r.bits.last === true.B)
        when(io.mem_master.r.bits.last) {
          state := sIdle
        }
      }
    }
  }

  // --------------------------
  // formal
  // --------------------------
  when(io.flush) {
    assume(io.resp.ready === false.B)
    assume(io.mmu_req.ready === false.B)

    assert(!io.resp.fire)
    assert(!io.req.fire)
    assert(!io.mmu_resp.fire)
    assert(!io.mmu_req.fire)
  }

  when(io.mmu_resp.fire) {
    assume(io.mmu_resp.bits.req_type === TLBReqType.Fetch)
  }

  when(io.mem_master.r.fire) {
    assume(io.mem_master.r.bits.id === 0.U)
    assume(io.mem_master.r.bits.resp === AXIDef.RESP_OKAY)
//    assume(io.mem_master.r.bits.last === true.B)
    assume(io.mem_master.r.bits.user === 0.U)
  }

  if (formal) {
    val f_flush = io.flush | past(io.flush)

    when(FormalUtils.StreamShouldStable(io.req) && !f_flush) {
      assume(io.req.valid)
      assume(stable(io.req.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mmu_req) && !f_flush) {
      assert(io.mmu_req.valid)
      assert(stable(io.mmu_req.bits))
    }
    when(FormalUtils.StreamShouldStable(io.resp) && !f_flush) {
      assert(io.resp.valid)
      assert(stable(io.resp.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mmu_resp) && !f_flush) {
      assume(io.mmu_resp.valid)
      assume(stable(io.mmu_resp.bits))
    }

    when(FormalUtils.StreamShouldStable(io.mem_master.ar)) {
      assert(io.mem_master.ar.valid)
      assert(stable(io.mem_master.ar.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.aw)) {
      assert(io.mem_master.aw.valid)
      assert(stable(io.mem_master.aw.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.w)) {
      assert(io.mem_master.w.valid)
      assert(stable(io.mem_master.w.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.r)) {
      assume(io.mem_master.r.valid)
      assume(stable(io.mem_master.r.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.b)) {
      assume(io.mem_master.b.valid)
      assume(stable(io.mem_master.b.bits))
    }
  }
}

class ICacheFormal extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "ICache" should "pass with assumption" in {
    verify(
      new ICacheTop(formal = true),
      Seq(BoundedCheck(10), CVC4EngineAnnotation)
    )
  }
}

object gen_icache_top_verilog extends App {
  GenVerilogHelper(new ICacheTop)
}
