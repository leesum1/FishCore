package leesum
import chisel3._
import chisel3.util.random.LFSR
import chisel3.util.{Decoupled, Enum, is, switch}

object TLBReqType extends ChiselEnum {
  val LOAD, STORE, fetchEntry = Value

}

class TLBReq extends Bundle {
  val vaddr = UInt(64.W)
  val req_type = TLBReqType()
}

class TLBResp extends Bundle {
  val paddr = UInt(64.W)
  val req_type = TLBReqType()
  val exception = new ExceptionEntry()

}

class DummyTLB extends Module {
  val io = IO(new Bundle {
    val tlb_req = Flipped(Decoupled(new TLBReq))
    val tlb_resp = Decoupled(new TLBResp)
    val flush = Input(Bool())
  })
  val sIdle :: sWaitResp :: sFlush :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val latency_cnt = RegInit(0.U(4.W))
  val req_buf = RegInit(0.U.asTypeOf(new TLBReq))
  val new_latency = LFSR(4, io.tlb_req.fire)

  io.tlb_req.ready := false.B
  io.tlb_resp.valid := false.B
  io.tlb_resp.bits := DontCare

  def recv_tlb_req(): Unit = {
    io.tlb_req.ready := true.B
    when(io.tlb_req.fire) {
      req_buf := io.tlb_req.bits
      state := sWaitResp
      latency_cnt := 0.U
    }.otherwise {
      state := sIdle
    }
  }

  def send_tlb_resp(): Unit = {
    io.tlb_resp.valid := true.B
    when(io.tlb_resp.fire) {
      io.tlb_resp.bits.req_type := req_buf.req_type
      io.tlb_resp.bits.paddr := req_buf.vaddr
      io.tlb_resp.bits.exception.valid := false.B
      io.tlb_resp.bits.exception.tval := 0.U
      io.tlb_resp.bits.exception.cause := ExceptionCause.load_access
      recv_tlb_req()
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
}

object gen_DummyTLB_verilog extends App {
  GenVerilogHelper(new DummyTLB)
}
