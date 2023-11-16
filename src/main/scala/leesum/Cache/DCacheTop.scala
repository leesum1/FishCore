//package leesum.ICache
//
//import chisel3._
//import chisel3.util.{Decoupled, Enum, is, switch}
//import chiseltest.ChiselScalatestTester
//import chiseltest.formal.{
//  BoundedCheck,
//  CVC4EngineAnnotation,
//  Formal,
//  past,
//  stable
//}
//import leesum.axi4.{AXIDef, AXIMasterIO, StreamFork2}
//import leesum._
//import org.scalatest.flatspec.AnyFlatSpec
//
////class LoadDcacheReq extends Bundle {
////  val paddr = UInt(64.W)
////  val size = UInt(2.W)
////  val is_mmio = Bool()
////}
////class LoadDcacheResp extends Bundle {
////  // the valid data is indicated by the paddr, like AXI4
////  val data = UInt(64.W)
////  val exception = new ExceptionEntry(has_valid = true)
////}
////class StoreDcacheReq extends Bundle {
////  val paddr = UInt(64.W)
////  // wdata and wstrb indicate the valid data to be written, like AXI4
////  val wdata = UInt(64.W)
////  val wstrb = UInt(8.W)
////  val size = UInt(2.W)
////  val is_mmio = Bool()
////}
////class StoreDcacheResp extends Bundle {
////  val exception = new ExceptionEntry()
////}
//class DCacheReq extends Bundle {
//  val va = UInt(64.W)
//  val size = UInt(2.W)
//}
//class DCacheResp extends Bundle {
//  val payload = UInt(64.W)
//  val exception = new ExceptionEntry()
//}
//class DCacheTop(formal: Boolean = false) extends Module {
//
//  val io = IO(new Bundle {
//    val req = Flipped(Decoupled(new ICacheReq))
//    val resp = Decoupled(new ICacheResp)
//    val flush = Input(Bool())
//    // mmu port
//    val mmu_req = Decoupled(new TLBReq)
//    val mmu_resp = Flipped(Decoupled(new TLBResp))
//    // memory port
//    val mem_master = new AXIMasterIO(64, 64)
//  })
//
//  val icache_hit = WireInit(false.B)
//  val icache_data = RegInit(0.U(64.W))
//  val icache_tag = RegInit(0.U(64.W))
//
//  io.mem_master.clear()
//  io.mem_master.ar.bits.id := 0.U
//  io.mem_master.ar.bits.lock := false.B
//  io.mem_master.ar.bits.cache := 0.U
//  io.mem_master.ar.bits.prot := 0.U
//  io.mem_master.ar.bits.qos := 0.U
//  io.mem_master.ar.bits.region := 0.U
//  io.mem_master.ar.bits.user := 0.U
//
//  io.mem_master.aw.bits.id := 0.U
//  io.mem_master.aw.bits.lock := false.B
//  io.mem_master.aw.bits.cache := 0.U
//  io.mem_master.aw.bits.prot := 0.U
//  io.mem_master.aw.bits.qos := 0.U
//  io.mem_master.aw.bits.region := 0.U
//  io.mem_master.aw.bits.user := 0.U
//
//  io.req.nodeq()
//  io.resp.noenq()
//  io.mmu_req.noenq()
//  io.mmu_resp.nodeq()
//
//  val sIdle :: sWaitTLBResp :: sSendICacheResp :: sSendRefillReq :: sWaitRefillResp :: sFlushRefillReq :: sFlushRefillResp :: Nil =
//    Enum(7)
//
//  val state = RegInit(sIdle)
//  val icache_req_buf = RegInit(0.U.asTypeOf(new ICacheReq))
//  val mmu_resp_buf = RegInit(0.U.asTypeOf(new TLBResp))
//
//  switch(state) {
//    is(sIdle) {
//      val (icache_req_fork, mmu_req_fork) =
//        StreamFork2(io.req, synchronous = true)
//
//      icache_req_fork.ready := !io.flush
//      mmu_req_fork.ready := io.mmu_req.ready
//      // Send mmu request
//      io.mmu_req.valid := mmu_req_fork.valid
//      io.mmu_req.bits.req_type := TLBReqType.Fetch
//      io.mmu_req.bits.vaddr := icache_req_fork.bits.va
//      io.mmu_req.bits.size := DcacheConst.SIZE8
//
//      when(io.req.fire) {
//        icache_req_buf := io.req.bits
//        state := sWaitTLBResp
//        // TODO: send read req of icache tag and data, resp at the next cycle
//        icache_data := 1122334.U // set a random value
//        icache_tag := 5566778.U // set a random value
//
//      }.otherwise {
//        state := sIdle
//      }
//    }
//    is(sWaitTLBResp) {
//      io.mmu_resp.ready := true.B && !io.flush
//      // TODO: not implemented now
//
//      when(io.mmu_resp.fire) {
//        assert(io.mmu_resp.bits.req_type === TLBReqType.Fetch)
//        icache_hit := io.mmu_resp.bits.paddr === icache_tag
//        mmu_resp_buf := io.mmu_resp.bits
//        // wait for mmu response
//        // at the same time, icache tag and data are ready, compare paddr with tag to check if hit
//        when(icache_hit) {
//          // TODO: not implemented now
//          // send response at the same cycle
//          io.resp.valid := true.B
//          io.resp.bits.payload := icache_data
//          io.resp.bits.exception := io.mmu_resp.bits.exception
//          when(io.resp.fire) {
//            // TODO: back by back
//            state := sIdle
//          }.otherwise {
//            // FetchStage can't accept resp at the same cycle
//            state := sSendICacheResp
//          }
//        }.otherwise {
//          // not hit, send refill request
//          // TODO: send refill request at the same cycle?
//          state := sSendRefillReq
//        }
//      }.elsewhen(io.flush) {
//        state := sIdle
//      }
//    }
//
//    is(sSendRefillReq) {
//      assert(mmu_resp_buf.req_type === TLBReqType.Fetch)
//      assert(CheckAligned(mmu_resp_buf.paddr, DcacheConst.SIZE8))
//
//      io.mem_master.ar.valid := true.B
//      io.mem_master.ar.bits.addr := mmu_resp_buf.paddr
//      io.mem_master.ar.bits.size := DcacheConst.SIZE8
//      io.mem_master.ar.bits.burst := 0.U
//      io.mem_master.ar.bits.len := 0.U
//      io.mem_master.ar.bits.id := 0.U // TODO: not implemented now
//
//      when(io.mem_master.ar.fire && !io.flush) {
//        state := sWaitRefillResp
//      }.elsewhen(io.mem_master.ar.fire && io.flush) {
//        // TODO: not implemented now
//        state := sFlushRefillResp
//      }.elsewhen(!io.mem_master.ar.fire && io.flush) {
//        state := sFlushRefillReq
//      }
//
//    }
//    is(sWaitRefillResp) {
//      assert(mmu_resp_buf.req_type === TLBReqType.Fetch)
//      assert(CheckAligned(mmu_resp_buf.paddr, DcacheConst.SIZE8))
//
//      io.mem_master.r.ready := true.B
//      when(io.mem_master.r.fire && !io.flush) {
//        // success, no flush
//        icache_data := io.mem_master.r.bits.data
//        icache_tag := mmu_resp_buf.paddr
//        // TODO: not implemented burst now
//        assert(io.mem_master.r.bits.last === true.B)
//
//        when(io.mem_master.r.bits.last) {
//          state := sSendICacheResp
//        }
//      }.elsewhen(
//        io.mem_master.r.fire && !io.mem_master.r.bits.last && io.flush
//      ) {
//        // encounter flush, but not last, go sFlushRefillResp wait for last
//        state := sFlushRefillResp
//      }
//    }
//
//    is(sSendICacheResp) {
//      icache_hit := mmu_resp_buf.paddr === icache_tag
//      assert(icache_hit)
//
//      io.resp.valid := true.B
//      io.resp.bits.payload := icache_data
//      io.resp.bits.exception := mmu_resp_buf.exception
//      when(io.resp.fire && !io.flush) {
//        assert(!io.flush)
//        // TODO: back by back
//        state := sIdle
//      }.elsewhen(io.flush) {
//        // cancel the response
//        state := sIdle
//      }
//    }
//
//    is(sFlushRefillReq) {
//      assert(mmu_resp_buf.req_type === TLBReqType.Fetch)
//      assert(CheckAligned(mmu_resp_buf.paddr, DcacheConst.SIZE8))
//      io.mem_master.ar.valid := true.B
//      io.mem_master.ar.bits.addr := mmu_resp_buf.paddr
//      io.mem_master.ar.bits.size := DcacheConst.SIZE8
//      io.mem_master.ar.bits.burst := 0.U
//      io.mem_master.ar.bits.len := 0.U
//      io.mem_master.ar.bits.id := 0.U // TODO: not implemented now
//      when(io.mem_master.ar.fire) {
//        state := sFlushRefillResp
//      }
//    }
//    is(sFlushRefillResp) {
//      assert(mmu_resp_buf.req_type === TLBReqType.Fetch)
//      assert(CheckAligned(mmu_resp_buf.paddr, DcacheConst.SIZE8))
//      io.mem_master.r.ready := true.B
//      when(io.mem_master.r.fire) {
//        assert(io.mem_master.r.bits.last === true.B)
//        when(io.mem_master.r.bits.last) {
//          state := sIdle
//        }
//      }
//    }
//  }
//
//  // --------------------------
//  // formal
//  // --------------------------
//  when(io.flush) {
//    assume(io.resp.ready === false.B)
//    assume(io.mmu_req.ready === false.B)
//
//    assert(!io.resp.fire)
//    assert(!io.req.fire)
//    assert(!io.mmu_resp.fire)
//    assert(!io.mmu_req.fire)
//  }
//
//  when(io.mmu_resp.fire) {
//    assume(io.mmu_resp.bits.req_type === TLBReqType.Fetch)
//    assume(CheckAligned(io.mmu_resp.bits.paddr, DcacheConst.SIZE8))
//  }
//
//  when(io.mem_master.r.fire) {
//    assume(io.mem_master.r.bits.id === 0.U)
//    assume(io.mem_master.r.bits.resp === AXIDef.RESP_OKAY)
//    assume(io.mem_master.r.bits.last === true.B)
//    assume(io.mem_master.r.bits.user === 0.U)
//  }
//
//  when(io.mem_master.r.fire) {
//    assume(io.mem_master.r.bits.last === true.B)
//  }
//
//  if (formal) {
//    val f_flush = io.flush | past(io.flush)
//
//    when(FormalUtils.StreamShouldStable(io.req) && !f_flush) {
//      assume(io.req.valid)
//      assume(stable(io.req.bits))
//    }
//    when(FormalUtils.StreamShouldStable(io.mmu_req) && !f_flush) {
//      assert(io.mmu_req.valid)
//      assert(stable(io.mmu_req.bits))
//    }
//    when(FormalUtils.StreamShouldStable(io.resp) && !f_flush) {
//      assert(io.resp.valid)
//      assert(stable(io.resp.bits))
//    }
//    when(FormalUtils.StreamShouldStable(io.mmu_resp) && !f_flush) {
//      assume(io.mmu_resp.valid)
//      assume(stable(io.mmu_resp.bits))
//    }
//
//    when(FormalUtils.StreamShouldStable(io.mem_master.ar)) {
//      assert(io.mem_master.ar.valid)
//      assert(stable(io.mem_master.ar.bits))
//    }
//    when(FormalUtils.StreamShouldStable(io.mem_master.aw)) {
//      assert(io.mem_master.aw.valid)
//      assert(stable(io.mem_master.aw.bits))
//    }
//    when(FormalUtils.StreamShouldStable(io.mem_master.w)) {
//      assert(io.mem_master.w.valid)
//      assert(stable(io.mem_master.w.bits))
//    }
//    when(FormalUtils.StreamShouldStable(io.mem_master.r)) {
//      assume(io.mem_master.r.valid)
//      assume(stable(io.mem_master.r.bits))
//    }
//    when(FormalUtils.StreamShouldStable(io.mem_master.b)) {
//      assume(io.mem_master.b.valid)
//      assume(stable(io.mem_master.b.bits))
//    }
//  }
//}
//
//class DCacheFormal extends AnyFlatSpec with ChiselScalatestTester with Formal {
//  "DCache" should "pass with assumption" in {
//    verify(
//      new DCacheTop(formal = true),
//      Seq(BoundedCheck(20), CVC4EngineAnnotation)
//    )
//  }
//}
//
//object gen_dcache_top_verilog extends App {
//  GenVerilogHelper(new DCacheTop)
//}
