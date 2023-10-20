package leesum.axi4
import chisel3._
import chisel3.util.{Enum, is, switch}
import leesum.axi4.AXIDef._

class AxiReadArbiter extends Module {

  val io = IO(new Bundle {
    val in = Vec(2, new AXISlaveIO(32, 64))
    val out = new AXIMasterIO(32, 64)
  })

  // -----------------------
  // axi read arbiter
  // -----------------------
  val r_occupied_sel_buf = RegInit(0.U(2.W))
  val sReadIdle :: sWaitResp :: Nil = Enum(2)
  val r_state = RegInit(sReadIdle)

  val r_sel_idx = VecInit(io.in.map(_.ar.valid)).indexWhere(_ === true.B)
  val r_sel_idx_valid = io.in.map(_.ar.valid).reduce(_ || _)

  def select_input(): Unit = {
    when(r_sel_idx_valid) {
      assert(r_sel_idx < 2.U, "idx must less than num_input")
      assert(io.in(r_sel_idx).ar.valid, "in_req(idx) must be valid")
      io.out.ar <> io.in(r_sel_idx).ar
      when(io.out.ar.fire) {
        r_occupied_sel_buf := r_sel_idx
        r_state := sWaitResp
      }.otherwise {
        r_state := sReadIdle
      }
    }.otherwise {
      r_state := sReadIdle
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

  switch(r_state) {
    is(sReadIdle) {
      select_input()
    }
    is(sWaitResp) {
      io.in(r_occupied_sel_buf).r <> io.out.r
      when(io.out.r.fire) {
        select_input()
      }
    }
  }
}
