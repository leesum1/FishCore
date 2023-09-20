package leesum
import chisel3._
import chisel3.util.{Decoupled, Enum, MuxLookup, PopCount, Queue, is, switch}

class AGUIn extends Bundle {
  val op_a = UInt(64.W)
  val op_b = UInt(64.W)
  val size = UInt(2.W)
  val store_data = UInt(64.W)
  val is_store = Bool()
}
class AGUOut extends Bundle {
  val load_pipe = Decoupled(new LoadQueueIn())
  val store_pipe = Decoupled(new StoreQueueIn())
  val exception_pipe = Decoupled(new ExceptionEntry())

  def agu_out_assert(): Unit = {
    assert(
      PopCount(
        Seq(load_pipe.valid, store_pipe.valid, exception_pipe.valid)
      ) <= 1.U,
      "AGU_out_assert error"
    )
  }
}

class AGU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AGUIn))
    val out = new AGUOut
    val flush = Input(Bool())
    // tlb interface
    val tlb_req = Decoupled(new TLBReq)
    val tlb_resp = Flipped(Decoupled(new TLBResp))
  })

  val vaddr = io.in.bits.op_a.asSInt + io.in.bits.op_b.asSInt

  val sIdle :: sWaitTLBRsp :: sWaitFifo :: sFlush :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val tlb_rsp_buf = RegInit(0.U.asTypeOf(new TLBResp))
  val size_buf = RegInit(0.U(2.W))
  val store_data_buf = RegInit(0.U(64.W))

  val exception_pipe = Module(new PipeLine(new ExceptionEntry()))

//  val load_pipe = Module(new PipeLine(UInt(64.W)))
//  val store_pipe = Module(new PipeLine(UInt(64.W)))

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
      size_buf := io.in.bits.size
      store_data_buf := io.in.bits.store_data
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
      exception_pipe.io.in.bits.valid := true.B
      exception_pipe.io.in.bits.tval := tlb_rsp.paddr

      val mis_aligned_cause = Mux(
        tlb_rsp.req_type === TLBReqType.LOAD,
        ExceptionCause.misaligned_load,
        ExceptionCause.misaligned_store
      )
      // page fault or access fault has higher priority
      exception_pipe.io.in.bits.cause := Mux(
        tlb_rsp.exception.valid,
        tlb_rsp.exception.cause,
        mis_aligned_cause
      )
      receive_tlb_req()
    }.otherwise {
      state := sWaitFifo
    }
  }
  def dispatch_to_load(tlb_rsp: TLBResp): Unit = {
    io.out.load_pipe.valid := true.B
    when(io.out.load_pipe.fire) {
      io.out.load_pipe.bits.paddr := tlb_rsp.paddr
      io.out.load_pipe.bits.size := size_buf
      // TODO: is_mmio is not implemented
      io.out.load_pipe.bits.is_mmio := false.B
      receive_tlb_req()
    }.otherwise {
      state := sWaitFifo
    }
  }
  def dispatch_to_store(tlb_rsp: TLBResp): Unit = {
    io.out.store_pipe.valid := true.B
    when(io.out.store_pipe.fire) {
      io.out.store_pipe.bits.paddr := tlb_rsp.paddr
      io.out.store_pipe.bits.size := size_buf
      io.out.store_pipe.bits.store_data := store_data_buf
      io.out.store_pipe.bits.is_mmio := false.B
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
        // flush is false, and tlb_resp is fire
        tlb_rsp_buf := io.tlb_resp.bits
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
        // flush is true, and tlb_resp is fire
        state := sIdle
      }.elsewhen(!io.tlb_resp.fire && io.flush) {
        // flush is true, but tlb_resp is not fire
        state := sIdle
      }.otherwise {
        state := sWaitTLBRsp
      }
    }
    is(sWaitFifo) {
      when(!io.flush) {
        when(check_privilege(tlb_rsp_buf)) {
          dispatch_to_exception(tlb_rsp_buf)
        }.elsewhen(io.tlb_resp.bits.req_type === TLBReqType.STORE) {
          dispatch_to_store(tlb_rsp_buf)
        }.elsewhen(io.tlb_resp.bits.req_type === TLBReqType.LOAD) {
          dispatch_to_load(tlb_rsp_buf)
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
