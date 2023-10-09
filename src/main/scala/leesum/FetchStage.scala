package leesum
import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Enum, Queue, is, switch}
import axi4.{AXI4Memory, AXIDef, AXIMasterIO, StreamFork, StreamFork2}

class FetchResp extends Bundle {
  val pc = UInt(64.W)
  val data = UInt(64.W)
  val exception = new ExceptionEntry()
}

class FetchStage extends Module {

  val io = IO(new Bundle {
    val pc_in = Flipped(Decoupled(UInt(64.W)))
    val tlb_req = Decoupled(new TLBReq)
    val tlb_resp = Flipped(Decoupled(new TLBResp))
    val icache_req = Decoupled(new LoadDcacheReq)
    val icache_resp = Flipped(Decoupled(new LoadDcacheResp))

    val fetch_resp = Decoupled(new FetchResp)
    val flush = Input(Bool())
  })

  val tlb_fifo_enq = Wire(DecoupledIO(new TLBResp))
  val tlb_fifo_deq =
    Queue(tlb_fifo_enq, 4, flow = true, flush = Some(io.flush))

  val fetch_resp_fifo_enq = Wire(DecoupledIO(new FetchResp))
  val fetch_resp_fifo_deq =
    Queue(fetch_resp_fifo_enq, 4, flow = true, flush = Some(io.flush))

  io.fetch_resp <> fetch_resp_fifo_deq

  tlb_fifo_enq.noenq()
  fetch_resp_fifo_enq.noenq()
  tlb_fifo_deq.nodeq()
  io.pc_in.nodeq()
  io.tlb_req.noenq()
  io.tlb_resp.nodeq()
  io.icache_req.noenq()
  io.icache_resp.nodeq()

  // --------------------------
  // TLB fsm logic
  // --------------------------
  val sIdle :: sWaitTLBResp :: sFlush :: Nil = Enum(3)
  val tlb_state = RegInit(sIdle)

  def send_tlb_req() = {
    io.pc_in.ready := io.tlb_req.ready
    io.tlb_req.valid := io.pc_in.valid && !io.flush

    io.tlb_req.bits.req_type := TLBReqType.Fetch
    io.tlb_req.bits.vaddr := io.pc_in.bits
    io.tlb_req.bits.size := GenSizeByAddr(io.pc_in.bits)

    when(io.pc_in.fire && io.tlb_req.fire) {
      tlb_state := sWaitTLBResp
    }.otherwise {
      tlb_state := sIdle
    }
  }

  switch(tlb_state) {
    is(sIdle) {
      send_tlb_req()
    }
    is(sWaitTLBResp) {
      io.tlb_resp <> tlb_fifo_enq
      when(io.tlb_resp.fire && !io.flush) {
        // 1. flush: false, tlb_resp.fire: true
        send_tlb_req()
      }.elsewhen(io.tlb_resp.fire && io.flush) {
        // 2. flush: true, tlb_resp.fire: true,discard the coming data
        tlb_state := sIdle
      }.elsewhen(!io.tlb_resp.fire && io.flush) {
        // 3. flush: ture, tlb_resp.fire: false
        tlb_state := sFlush
      }.otherwise {
        // 4. flush: false, tlb_resp.fire: false
        tlb_state := sWaitTLBResp
      }
    }
    is(sFlush) {
      io.tlb_resp.ready := true.B
      when(io.tlb_resp.fire) {
        // discard the coming data
        tlb_state := sIdle
      }
    }
  }

  // --------------------------
  // Icache  fsm logic
  // --------------------------

  val sIcacheIdle :: sWaitIcacheResp :: sIcacheFlush :: Nil = Enum(3)

  val Icache_state = RegInit(sIcacheIdle)

  val tlb_buf = RegInit(0.U.asTypeOf(new TLBResp))

  // TODO: NOT IMPLEMENTED
  def check_mmio(tlb_resp: TLBResp): Bool = {
    false.B
  }

  def send_icache_req() = {
    when(tlb_fifo_deq.valid && !io.flush) {
      assert(tlb_fifo_deq.bits.req_type === TLBReqType.Fetch)
      assert(tlb_fifo_deq.bits.exception.valid === false.B)

      val is_mmio = check_mmio(tlb_fifo_deq.bits)
      io.icache_req.valid := true.B
      io.icache_req.bits.paddr := tlb_fifo_deq.bits.paddr
      io.icache_req.bits.size := tlb_fifo_deq.bits.size
      io.icache_req.bits.is_mmio := is_mmio

      when(io.icache_req.fire) {
        tlb_buf := tlb_fifo_deq.bits
        tlb_fifo_deq.ready := true.B
        Icache_state := sWaitIcacheResp
      }.otherwise {
        Icache_state := sIcacheIdle
      }
    }.otherwise {
      Icache_state := sIcacheIdle
    }
  }

  def bypass_exception() = {
    when(tlb_fifo_deq.valid && !io.flush) {
      assert(tlb_fifo_deq.bits.req_type === TLBReqType.Fetch)
      assert(tlb_fifo_deq.bits.exception.valid === true.B)
      fetch_resp_fifo_enq.valid := true.B
      fetch_resp_fifo_enq.bits.exception := tlb_fifo_deq.bits.exception
      fetch_resp_fifo_enq.bits.data := 0.U
      fetch_resp_fifo_enq.bits.pc := tlb_fifo_deq.bits.paddr

      when(fetch_resp_fifo_enq.fire) {
        tlb_fifo_deq.ready := true.B
      }
      Icache_state := sIcacheIdle
    }.otherwise {
      Icache_state := sIcacheIdle
    }
  }

  switch(Icache_state) {
    is(sIcacheIdle) {
      when(tlb_fifo_deq.bits.exception.valid) {
        bypass_exception()
      }.otherwise {
        send_icache_req()
      }
    }
    is(sWaitIcacheResp) {

      fetch_resp_fifo_enq.valid := io.icache_resp.valid
      fetch_resp_fifo_enq.bits.pc := tlb_buf.paddr
      fetch_resp_fifo_enq.bits.data := io.icache_resp.bits.data
      fetch_resp_fifo_enq.bits.exception.valid := false.B
      fetch_resp_fifo_enq.bits.exception.tval := 0.U
      fetch_resp_fifo_enq.bits.exception.cause := ExceptionCause.misaligned_fetch // 0
      io.icache_resp.ready := fetch_resp_fifo_enq.ready

      when(io.icache_resp.fire && !io.flush) {
        // 1. flush: false, icache_resp.fire: true
        when(tlb_fifo_deq.bits.exception.valid) {
          bypass_exception()
        }.otherwise {
          send_icache_req()
        }
      }.elsewhen(io.icache_resp.fire && io.flush) {
        // 2. flush: true, icache_resp.fire: true, discard the coming data
        Icache_state := sIcacheIdle
      }.elsewhen(!io.icache_resp.fire && io.flush) {
        // 3. flush: true, icache_resp.fire: false
        Icache_state := sIcacheFlush
      }.otherwise {
        // 4. flush: false, icache_resp.fire: false
        Icache_state := sWaitIcacheResp
      }
    }
    is(sIcacheFlush) {
      io.icache_resp.ready := true.B
      when(io.icache_resp.fire) {
        // discard the coming data
        Icache_state := sIcacheIdle
      }
    }
  }
}

object gen_fetch_stage_verilog extends App {
  GenVerilogHelper(new FetchStage)
}
