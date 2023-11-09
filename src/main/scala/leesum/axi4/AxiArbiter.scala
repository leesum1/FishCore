package leesum.axi4
import chisel3._
import chisel3.util.{Enum, is, switch}
import chiseltest.formal.past
import leesum.{GenVerilogHelper, ReqRespArbiter}

class AxiReadArbiter extends Module {

  val io = IO(new Bundle {
    val flush = Input(Bool())
    val in = Vec(2, new AXISlaveIO(32, 64))
    val out = new AXIMasterIO(32, 64)
  })

  io.in.foreach(_.ar.nodeq())
  io.in.foreach(_.r.noenq())
  io.in.foreach(_.aw.nodeq())
  io.in.foreach(_.w.nodeq())
  io.in.foreach(_.b.noenq())

  io.out.ar.noenq()
  io.out.r.nodeq()
  io.out.aw.noenq()
  io.out.w.noenq()
  io.out.b.nodeq()

  val axi_r_arb = Module(
    new ReqRespArbiter(2, new AXIAddressChannel(32), new AXIReadDataChannel(64))
  )

  axi_r_arb.io.flush := io.flush
  axi_r_arb.io.req_vec <> io.in.map(_.ar)
  axi_r_arb.io.resp_vec <> io.in.map(_.r)
  io.out.ar <> axi_r_arb.io.req_arb
  io.out.r <> axi_r_arb.io.resp_arb

}

class AXIWriteArbiter extends Module {

  val io = IO(new Bundle {
    val in = Vec(2, new AXISlaveIO(32, 64))
    val out = new AXIMasterIO(32, 64)
  })

  // -----------------------
  // axi write arbiter
  // -----------------------
  val w_occupied_sel_buf = RegInit(0.U(2.W))
  val sWriteIdle :: sWaitData :: sWaitResp :: Nil = Enum(2)
  val w_state = RegInit(sWriteIdle)

  val w_sel_idx = VecInit(io.in.map(_.aw.valid)).indexWhere(_ === true.B)
  val w_sel_idx_valid = io.in.map(_.aw.valid).reduce(_ || _)

  def select_input(): Unit = {
    when(w_sel_idx_valid) {
      assert(w_sel_idx < 2.U, "idx must less than num_input")
      assert(io.in(w_sel_idx).aw.valid, "in_req(idx) must be valid")
      io.out.aw <> io.in(w_sel_idx).aw
      when(io.out.aw.fire) {
        w_occupied_sel_buf := w_sel_idx
        w_state := sWaitResp
      }.otherwise {
        w_state := sWriteIdle
      }
    }.otherwise {
      w_state := sWriteIdle
    }
  }

  io.in.foreach(_.ar.nodeq())
  io.in.foreach(_.r.noenq())
  io.in.foreach(_.aw.nodeq())
  io.in.foreach(_.w.nodeq())
  io.in.foreach(_.b.noenq())

  io.out.ar.noenq()
  io.out.r.nodeq()
  io.out.aw.noenq()
  io.out.w.noenq()
  io.out.b.nodeq()

  switch(w_state) {
    is(sWriteIdle) {
      select_input()
    }

    is(sWaitData) {
      io.in(w_occupied_sel_buf).w <> io.out.w
      when(io.out.w.fire) {
        when(io.out.w.bits.last) {
          w_state := sWaitResp
        }
      }
    }
    is(sWaitResp) {
      io.in(w_occupied_sel_buf).b <> io.out.b
      when(io.out.b.fire) {
        select_input()
      }
    }
  }
}
