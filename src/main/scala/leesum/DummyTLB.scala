package leesum
import chisel3._
import chisel3.util.random.LFSR
import chisel3.util.{Decoupled, Enum, MixedVecInit, is, switch}
import chiseltest.ChiselScalatestTester
import chiseltest.experimental.observe
import chiseltest.formal.{BoundedCheck, Formal, fell, past, rose, stable}
import org.scalatest.flatspec.AnyFlatSpec

object TLBReqType extends ChiselEnum {
  val LOAD = Value(0.U)
  val STORE = Value(1.U)
  val Fetch = Value(2.U)
  val AMO = Value(3.U)
  val LR = Value(4.U)
  val SC = Value(5.U)

  def need_load(req_type: TLBReqType.Type): Bool = {
    req_type === LOAD || req_type === LR
  }

  def need_store(req_type: TLBReqType.Type): Bool = {
    req_type === STORE || req_type === AMO || req_type === SC
  }

  def is_atomic(req_type: TLBReqType.Type): Bool = {
    req_type === LR || req_type === AMO || req_type === SC
  }
}

class TLBReq extends Bundle {
  val vaddr = UInt(64.W)
  val size = UInt(2.W)
  val req_type = TLBReqType()
}

class TLBResp extends Bundle {
  val paddr = UInt(64.W)
  val req_type = TLBReqType()
  // size: 0: 1 byte, 1: 2 bytes, 2: 4 bytes, 3: 8 bytes
  val size = UInt(2.W)
  val exception = new ExceptionEntry()
}

class DummyTLB(random_latency: Boolean = true, formal: Boolean = false)
    extends Module {
  val io = IO(new Bundle {
    val tlb_req = Flipped(Decoupled(new TLBReq))
    val tlb_resp = Decoupled(new TLBResp)
    // TODO: need flush?
    val flush = Input(Bool())
  })
  val sIdle :: sWaitResp :: sFlush :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val latency_cnt = RegInit(0.U(4.W))
  val req_buf = RegInit(0.U.asTypeOf(new TLBReq))
  val new_latency = if (random_latency) {
    LFSR(4, io.tlb_req.fire)
  } else {
    0.U
  }

  io.tlb_req.ready := false.B
  io.tlb_resp.valid := false.B
  io.tlb_resp.bits := DontCare

  def recv_tlb_req(): Unit = {
    io.tlb_req.ready := true.B && !io.flush
    when(io.tlb_req.fire && !io.flush) {
      req_buf := io.tlb_req.bits
      state := sWaitResp
      latency_cnt := 0.U
    }.otherwise {
      state := sIdle
    }
  }

  def send_tlb_resp(): Unit = {
    io.tlb_resp.valid := true.B
    io.tlb_resp.bits.req_type := req_buf.req_type
    io.tlb_resp.bits.paddr := req_buf.vaddr
    io.tlb_resp.bits.size := req_buf.size
    // TODO: not implemented, page fault or access fault
    io.tlb_resp.bits.exception.valid := false.B
    io.tlb_resp.bits.exception.tval := 0.U
    // TODO:
    io.tlb_resp.bits.exception.cause := ExceptionCause.load_access

    when(io.tlb_resp.fire && !io.flush) {
      recv_tlb_req() // back by back
    }.elsewhen(io.flush) {
      state := sFlush
    }.otherwise {
      state := sWaitResp
    }
  }

  switch(state) {
    is(sIdle) {
      recv_tlb_req()
    }
    is(sWaitResp) {
      when(latency_cnt === new_latency) {
        send_tlb_resp()
      }.otherwise {
        latency_cnt := latency_cnt + 1.U
      }
    }
  }

  // --------------------------
  // formal
  // --------------------------

  if (formal) {
    when(FormalUtils.StreamShouldStable(io.tlb_req)) {
      assume(io.tlb_req.valid)
      assume(stable(io.tlb_req.bits))
    }

    when(FormalUtils.StreamShouldStable(io.tlb_resp)) {
      assert(io.tlb_resp.valid)
      assert(stable(io.tlb_resp.bits))
    }

  }
}

class DummyTLBFormal
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Formal {
  "DummyTLBFormal" should "pass with assumption" in {
    verify(new DummyTLB(formal = true), Seq(BoundedCheck(50)))
  }
}

object gen_DummyTLB_verilog extends App {
  GenVerilogHelper(new DummyTLB)
}
