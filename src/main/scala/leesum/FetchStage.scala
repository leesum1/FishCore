//package leesum
//import chisel3._
//import chisel3.util.{Decoupled, DecoupledIO, Enum, Queue, is, switch}
//import circt.stage.ChiselStage
//
//class FetchResp extends Bundle {
//  val pc = UInt(64.W)
//  val data = UInt(64.W)
//  val exception = new ExceptionEntry()
//}
//
//class FetchStage extends Module {
//
//  val io = IO(new Bundle {
//    val pc_in = Flipped(Decoupled(UInt(64.W)))
//    val tlb_req = Decoupled(new TLBReq)
//    val tlb_resp = Flipped(Decoupled(new TLBResp))
//    val icache_req = Decoupled(new LoadDcacheReq)
//    val icache_resp = Flipped(Decoupled(new LoadDcacheResp))
//
//    val fetch_resp = Decoupled(new FetchResp)
//    val flush = Input(Bool())
//  })
//
//  val tlb_resp_fifo = Module(
//    new Queue(new TLBResp, 16, flow = true, hasFlush = true)
//  )
//  val fetch_resp_fifo = Module(
//    new Queue(new FetchResp, 16, flow = true, hasFlush = true)
//  )
//  val tlb_fifo_almost_full = tlb_resp_fifo.io.count > 14.U
//  val fetch_resp_fifo_almost_full = fetch_resp_fifo.io.count > 14.U
//
//  tlb_resp_fifo.io.flush.get := io.flush
//  fetch_resp_fifo.io.flush.get := io.flush
//
//  io.fetch_resp <> fetch_resp_fifo.io.deq
//
//  tlb_resp_fifo.io.enq.noenq()
//  tlb_resp_fifo.io.deq.nodeq()
//  fetch_resp_fifo.io.enq.noenq()
//  io.pc_in.nodeq()
//  io.tlb_req.noenq()
//  io.tlb_resp.nodeq()
//  io.icache_req.noenq()
//  io.icache_resp.nodeq()
//
//  // --------------------------
//  // TLB fsm logic
//  // --------------------------
//  val sIdle :: sWaitTLBResp :: sFlush :: Nil = Enum(3)
//  val tlb_state = RegInit(sIdle)
//
//  def send_tlb_req() = {
//    io.pc_in.ready := !tlb_fifo_almost_full && !io.flush
//    io.tlb_req.valid := io.pc_in.valid && !tlb_fifo_almost_full
//
//    io.tlb_req.bits.req_type := TLBReqType.Fetch
//    io.tlb_req.bits.vaddr := io.pc_in.bits
//    io.tlb_req.bits.size := GenSizeByAddr(io.pc_in.bits)
//
//    when(io.pc_in.fire && io.tlb_req.fire) {
//      assert(!io.flush)
//      tlb_state := sWaitTLBResp
//    }.otherwise {
//      tlb_state := sIdle
//    }
//  }
//
//  switch(tlb_state) {
//    is(sIdle) {
//      send_tlb_req()
//    }
//    is(sWaitTLBResp) {
//      // TODO: ready  flush?
//      io.tlb_resp <> tlb_resp_fifo.io.enq
//
//      when(io.tlb_resp.fire && !io.flush) {
//        // 1. flush: false, tlb_resp.fire: true
//        send_tlb_req()
//      }.elsewhen(io.flush) {
//        // 2. flush: true, tlb_resp.fire: true,discard the coming data
//        tlb_state := sIdle
//      }.otherwise {
//        // 4. flush: false, tlb_resp.fire: false
//        tlb_state := sWaitTLBResp
//      }
//    }
//  }
//
//  // --------------------------
//  // Icache  fsm logic
//  // --------------------------
//
//  val sIcacheIdle :: sWaitIcacheResp :: sIcacheFlush :: Nil = Enum(3)
//
//  val Icache_state = RegInit(sIcacheIdle)
//
//  val tlb_buf = RegInit(0.U.asTypeOf(new TLBResp))
//
//  // TODO: NOT IMPLEMENTED
//  def check_mmio(tlb_resp: TLBResp): Bool = {
//    false.B
//  }
//
//  def send_icache_req() = {
//    when(
//      tlb_resp_fifo.io.deq.valid && !io.flush && !fetch_resp_fifo_almost_full
//    ) {
//      assert(tlb_resp_fifo.io.deq.bits.req_type === TLBReqType.Fetch)
//      assert(tlb_resp_fifo.io.deq.bits.exception.valid === false.B)
//
//      val is_mmio = check_mmio(tlb_resp_fifo.io.deq.bits)
//      io.icache_req.valid := true.B
//      io.icache_req.bits.paddr := tlb_resp_fifo.io.deq.bits.paddr
//      io.icache_req.bits.size := tlb_resp_fifo.io.deq.bits.size
//      io.icache_req.bits.is_mmio := is_mmio
//
//      when(io.icache_req.fire) {
//        tlb_buf := tlb_resp_fifo.io.deq.bits
//        tlb_resp_fifo.io.deq.ready := true.B
//        Icache_state := sWaitIcacheResp
//      }.otherwise {
//        Icache_state := sIcacheIdle
//      }
//    }.otherwise {
//      Icache_state := sIcacheIdle
//    }
//  }
//
//  def bypass_exception() = {
//    when(
//      tlb_resp_fifo.io.deq.valid && !io.flush && !fetch_resp_fifo_almost_full
//    ) {
//      assert(tlb_resp_fifo.io.deq.bits.req_type === TLBReqType.Fetch)
//      assert(tlb_resp_fifo.io.deq.bits.exception.valid === true.B)
//
//      fetch_resp_fifo.io.enq.valid := true.B
//      fetch_resp_fifo.io.enq.bits.exception := tlb_resp_fifo.io.deq.bits.exception
//      fetch_resp_fifo.io.enq.bits.data := 0.U
//      fetch_resp_fifo.io.enq.bits.pc := tlb_resp_fifo.io.deq.bits.paddr
//
//      when(fetch_resp_fifo.io.enq.fire) {
//        tlb_resp_fifo.io.deq.ready := true.B
//      }
//      Icache_state := sIcacheIdle
//    }.otherwise {
//      Icache_state := sIcacheIdle
//    }
//  }
//
//  switch(Icache_state) {
//    is(sIcacheIdle) {
//      when(tlb_resp_fifo.io.deq.bits.exception.valid) {
//        bypass_exception()
//      }.otherwise {
//        send_icache_req()
//      }
//    }
//    is(sWaitIcacheResp) {
//
//      fetch_resp_fifo.io.enq.valid := io.icache_resp.valid
//      fetch_resp_fifo.io.enq.bits.pc := tlb_buf.paddr
//      fetch_resp_fifo.io.enq.bits.data := io.icache_resp.bits.data
//      fetch_resp_fifo.io.enq.bits.exception.valid := false.B
//      fetch_resp_fifo.io.enq.bits.exception.tval := 0.U
//      fetch_resp_fifo.io.enq.bits.exception.cause := ExceptionCause.misaligned_fetch // 0
//      io.icache_resp.ready := fetch_resp_fifo.io.enq.ready
//
//      when(io.icache_resp.fire && !io.flush) {
//        // 1. flush: false, icache_resp.fire: true
//        when(tlb_resp_fifo.io.deq.bits.exception.valid) {
//          bypass_exception()
//        }.otherwise {
//          send_icache_req()
//        }
//      }.elsewhen(io.icache_resp.fire && io.flush) {
//        // 2. flush: true, icache_resp.fire: true, discard the coming data
//        Icache_state := sIcacheIdle
//      }.elsewhen(!io.icache_resp.fire && io.flush) {
//        // 3. flush: true, icache_resp.fire: false
//        Icache_state := sIcacheFlush
//      }.otherwise {
//        // 4. flush: false, icache_resp.fire: false
//        Icache_state := sWaitIcacheResp
//      }
//    }
//    is(sIcacheFlush) {
//      io.icache_resp.ready := true.B
//      when(io.icache_resp.fire) {
//        // discard the coming data
//        Icache_state := sIcacheIdle
//      }
//    }
//  }
//}
//
//object gen_fetch_stage_verilog extends App {
//  GenVerilogHelper(new FetchStage)
//}
