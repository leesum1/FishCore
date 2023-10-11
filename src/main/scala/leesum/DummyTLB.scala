package leesum
import chisel3._
import chisel3.util.random.LFSR
import chisel3.util.{Decoupled, Enum, MixedVecInit, is, switch}

object TLBReqType extends ChiselEnum {
  val LOAD, STORE, Fetch = Value

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
  val exception = new ExceptionEntry(has_valid = true)
}

class DummyTLB(random_latency: Boolean = true) extends Module {
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
      io.tlb_resp.bits.size := req_buf.size
      // TODO: not implemented, page fault or access fault
      io.tlb_resp.bits.exception.valid := false.B
      io.tlb_resp.bits.exception.tval := 0.U
      // TODO:
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

class TLBArbiter(num_input: Int) extends Module {
  val io = IO(new Bundle {
    val in_req = Vec(num_input, Flipped(Decoupled(new TLBReq)))
    val in_resp = Vec(num_input, Decoupled(new TLBResp))

    val out_req = Decoupled(new TLBReq)
    val out_resp = Flipped(Decoupled(new TLBResp))
  })
  val occupied_sel_buf = RegInit(0.U(num_input.W))

  val sIdle :: sWaitResp :: Nil = Enum(2)

  val state = RegInit(sIdle)

  val sel_idx = VecInit(io.in_req.map(_.valid)).indexWhere(_ === true.B)
  val sel_idx_valid = io.in_req.map(_.valid).reduce(_ || _)

  def sen_tlb_req(idx: UInt): Unit = {

    when(sel_idx_valid) {
      assert(idx < num_input.U, "idx must less than num_input")
      assert(io.in_req(idx).valid, "in_req(idx) must be valid")
      io.out_req <> io.in_req(idx)
      when(io.out_req.fire) {
        occupied_sel_buf := idx
        state := sWaitResp
      }.otherwise {
        state := sIdle
      }
    }.otherwise {
      state := sIdle
    }
  }

  io.in_req.foreach(_.nodeq())
  io.in_resp.foreach(_.noenq())
  io.out_req.noenq()
  io.out_resp.nodeq()

  switch(state) {
    is(sIdle) {
      sen_tlb_req(sel_idx)
    }
    is(sWaitResp) {
      io.in_resp(occupied_sel_buf) <> io.out_resp
      when(io.out_resp.fire) {
//        sen_tlb_req(sel_idx)
        state := sIdle
      }
    }
  }
}

object gen_DummyTLB_verilog extends App {
  GenVerilogHelper(new DummyTLB)
}

object gen_TLBArbiter_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val in = Input(Vec(15, Bool()))
      val out_idx1 = Output(UInt(8.W))
      val out_idx2 = Output(UInt(8.W))
      val out_val = Output(Bool())
    })

    io.out_idx1 := io.in.indexWhere(_ === true.B)

    val scalaVector = io.in.zipWithIndex
      .map(x => MixedVecInit(x._1, x._2.U(8.W)))

    val resFun2 = VecInit(scalaVector)
      .reduceTree((x, y) => Mux(x(0).asBool, x, y))

    io.out_val := resFun2(0).asBool
    io.out_idx2 := resFun2(1)
  })
}
