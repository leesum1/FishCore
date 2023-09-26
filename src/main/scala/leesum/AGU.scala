package leesum
import chisel3._
import chisel3.util.{Decoupled, Enum, PopCount, is, switch}

class AGUIn extends Bundle {
  val op_a = UInt(64.W)
  val op_b = UInt(64.W)
  val size = UInt(2.W)
  val store_data = UInt(64.W)
  val trans_id = UInt(32.W)
  val is_store = Bool()
  // need by load
  val sign_ext = Bool()
}

class ExceptionQueueIn extends Bundle {
  val trans_id = UInt(32.W)
  val exception = new ExceptionEntry()
}

class AGUOut extends Bundle {
  val load_pipe = Decoupled(new LoadQueueIn())
  val store_pipe = Decoupled(new StoreQueueIn())
  val exception_pipe = Decoupled(new ExceptionQueueIn())
}

class AGU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AGUIn))
    val out = new AGUOut
    val flush = Input(Bool())
    // tlb interface
    val tlb_req = Decoupled(new TLBReq)
    val tlb_resp = Flipped(Decoupled(new TLBResp))
    // from store bypass
    val store_bypass = Flipped(new StoreBypassIO)
  })

  val vaddr = io.in.bits.op_a.asSInt + io.in.bits.op_b.asSInt

  val sIdle :: sWaitTLBRsp :: sWaitFifo :: sFlush :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val tlb_resp_buf = RegInit(0.U.asTypeOf(new TLBResp))
  val agu_req_buf = RegInit(0.U.asTypeOf(new AGUIn))

  val exception_pipe = Module(new PipeLine(new ExceptionQueueIn()))

  io.tlb_req.valid := false.B
  io.tlb_resp.ready := false.B
  io.in.ready := false.B
  io.tlb_req.bits := DontCare

  exception_pipe.io.in.valid := false.B
  exception_pipe.io.in.bits := DontCare

  exception_pipe.io.flush := io.flush

  io.out.store_pipe.valid := false.B
  io.out.store_pipe.bits := DontCare
  io.out.load_pipe.valid := false.B
  io.out.load_pipe.bits := DontCare
  io.store_bypass.valid := false.B
  io.store_bypass.paddr := DontCare

  def send_tlb_req(virtual_addr: UInt, size: UInt): Unit = {
    io.tlb_req.valid := true.B
    io.tlb_req.bits.vaddr := virtual_addr
    io.tlb_req.bits.size := size
    io.tlb_req.bits.req_type := Mux(
      io.in.bits.is_store,
      TLBReqType.STORE,
      TLBReqType.LOAD
    )
  }

  def receive_tlb_req(): Unit = {
    io.in.ready := true.B && !io.flush
    when(io.in.fire) {
      state := sWaitTLBRsp
      agu_req_buf := io.in.bits
      send_tlb_req(vaddr.asUInt, io.in.bits.size)
    }.otherwise {
      state := sIdle
    }
  }
  // TODO: not implemented now
  def check_privilege(tlb_rsp: TLBResp): Bool = {
    val addr_align = CheckAligned(tlb_rsp.paddr, tlb_rsp.size)

    tlb_rsp.exception.valid || !addr_align
  }

  // TODO: not implemented now
  def dispatch_to_exception(tlb_rsp: TLBResp): Unit = {
    exception_pipe.io.in.valid := true.B
    when(exception_pipe.io.in.fire) {
      exception_pipe.io.in.bits.exception.valid := true.B
      exception_pipe.io.in.bits.exception.tval := tlb_rsp.paddr
      exception_pipe.io.in.bits.trans_id := agu_req_buf.trans_id

      val mis_aligned_cause = Mux(
        tlb_rsp.req_type === TLBReqType.LOAD,
        ExceptionCause.misaligned_load,
        ExceptionCause.misaligned_store
      )
      // page fault or access fault has higher priority
      exception_pipe.io.in.bits.exception.cause := Mux(
        tlb_rsp.exception.valid,
        tlb_rsp.exception.cause,
        mis_aligned_cause
      )
      // back to back
      receive_tlb_req()
    }.otherwise {
      state := sWaitFifo
    }
  }
  def dispatch_to_load(tlb_rsp: TLBResp): Unit = {
    // when current load addr conflict with uncommitted store addr, and the store is a mmio operation
    // then stall the current load.
    val need_stall = io.store_bypass.data.valid && io.store_bypass.data.is_mmio
    io.out.load_pipe.valid := !need_stall
    when(io.out.load_pipe.fire) {
      io.out.load_pipe.bits.paddr := tlb_rsp.paddr
      io.out.load_pipe.bits.size := agu_req_buf.size
      io.out.load_pipe.bits.trans_id := agu_req_buf.trans_id
      io.out.load_pipe.bits.store_bypass := io.store_bypass.data
      io.out.load_pipe.bits.sign_ext := agu_req_buf.sign_ext
      // TODO: is_mmio is not implemented
      io.out.load_pipe.bits.is_mmio := false.B
      // back to back
      receive_tlb_req()
    }.otherwise {
      state := sWaitFifo
    }
  }
  def dispatch_to_store(tlb_rsp: TLBResp): Unit = {
    // when current store addr conflict with uncommitted store addr.
    // than stall dispatch the current store
    io.out.store_pipe.valid := true.B && !io.store_bypass.data.valid
    when(io.out.store_pipe.fire) {
      io.out.store_pipe.bits.paddr := tlb_rsp.paddr
      io.out.store_pipe.bits.size := agu_req_buf.size

      // convert store_data to axi wdata
      io.out.store_pipe.bits.wdata := GenAxiWdata(
        agu_req_buf.store_data,
        tlb_rsp.paddr
      )
      io.out.store_pipe.bits.wstrb := GenAxiWstrb(
        tlb_rsp.paddr,
        agu_req_buf.size
      )
      io.out.store_pipe.bits.trans_id := agu_req_buf.trans_id
      // TODO: is_mmio is not implemented
      io.out.store_pipe.bits.is_mmio := false.B
      // back to back
      receive_tlb_req()
    }.otherwise {
      state := sWaitFifo
    }
  }

  switch(state) {
    is(sIdle) {
      receive_tlb_req()
    }
    // TODO: what if flush is true?
    is(sWaitTLBRsp) {
      io.tlb_resp.ready := true.B
      when(io.tlb_resp.fire && !io.flush) {
        // 1. flush is false, and tlb_resp is fire
        tlb_resp_buf := io.tlb_resp.bits
        // send request to StoreQueue in order to check store bypass
        io.store_bypass.valid := true.B
        io.store_bypass.paddr := io.tlb_resp.bits.paddr

        when(check_privilege(io.tlb_resp.bits)) {
          dispatch_to_exception(io.tlb_resp.bits)
        }.elsewhen(io.tlb_resp.bits.req_type === TLBReqType.STORE) {
          dispatch_to_store(io.tlb_resp.bits)
        }.elsewhen(io.tlb_resp.bits.req_type === TLBReqType.LOAD) {
          dispatch_to_load(io.tlb_resp.bits)
        }.otherwise {
          assert(false.B, "sWaitTLBRsp error, should not reach here")
        }
      }.elsewhen(io.tlb_resp.fire && io.flush) {
        // 2. flush is true, and tlb_resp is fire
        state := sIdle
      }.elsewhen(!io.tlb_resp.fire && io.flush) {
        // 3. flush is true, but tlb_resp is not fire
        state := sFlush
      }.otherwise {
        // 4. flush is false, and tlb_resp is not fire
        state := sWaitTLBRsp
      }
    }
    is(sWaitFifo) {
      when(!io.flush) {
        io.store_bypass.valid := true.B
        io.store_bypass.paddr := tlb_resp_buf.paddr
        when(check_privilege(tlb_resp_buf)) {
          dispatch_to_exception(tlb_resp_buf)
        }.elsewhen(io.tlb_resp.bits.req_type === TLBReqType.STORE) {
          dispatch_to_store(tlb_resp_buf)
        }.elsewhen(io.tlb_resp.bits.req_type === TLBReqType.LOAD) {
          dispatch_to_load(tlb_resp_buf)
        }.otherwise {
          assert(false.B, "sWaitFifo error, should not reach here")
        }
      }.otherwise {
        state := sIdle
      }
    }
    is(sFlush) {
      io.tlb_resp.ready := true.B
      when(io.tlb_resp.fire) {
        state := sIdle
      }
    }
  }

  io.out.exception_pipe <> exception_pipe.io.out

}

object gen_agu_verilog extends App {
  GenVerilogHelper(new AGU())
}
