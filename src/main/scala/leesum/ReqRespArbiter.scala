package leesum
import chisel3._
import chisel3.util.{
  Decoupled,
  DecoupledIO,
  Enum,
  MixedVec,
  MixedVecInit,
  Valid,
  is,
  log2Ceil,
  switch
}
import chiseltest.ChiselScalatestTester
import chiseltest.formal.{BoundedCheck, Formal, stable}
import org.scalatest.flatspec.AnyFlatSpec

/** This Module is a generic arbiter for request and response signals.The lowest
  * index input has the highest priority. if no flush signal, the arbiter will
  * always response the request (such as axi).
  * @param numInputs
  * @param reqType
  * @param respType
  * @tparam T
  * @tparam U
  */
class ReqRespArbiter[T <: Data, U <: Data](
    numInputs: Int,
    reqType: T,
    respType: U,
    formal: Boolean = false
) extends Module {

  override def desiredName: String = "Arb_%d_%s_%s".format(
    numInputs,
    reqType.toString,
    respType.toString
  )

  val io = IO(new Bundle {

    val flush = Input(Bool())
    val req_vec = Vec(numInputs, Flipped(Decoupled(reqType)))
    val resp_vec = Vec(numInputs, Decoupled(respType))

    val req_arb = Decoupled(reqType)
    val resp_arb = Flipped(Decoupled(respType))
    val sel_idx = Output(Valid(UInt(log2Ceil(numInputs).W)))
  })

  val sIdle :: sWaitResp :: Nil = Enum(2)

  val state = RegInit(sIdle)

  val sel_buf_valid = state =/= sIdle
  val sel_buf = RegInit(0.U(log2Ceil(numInputs).W))
  val cur_max_priority = RegInit(0.U(log2Ceil(numInputs).W))
  val next_max_priority =
    Mux(cur_max_priority === (numInputs - 1).U, 0.U, cur_max_priority + 1.U)

  val sel_idx_valid = io.req_vec.map(_.valid).reduce(_ || _)

  // -------------------
  // priority
  // -------------------
//  val sel_idx = VecInit(io.req_vec.map(_.valid))
//    .indexWhere(_ === true.B)

  // -------------------
  // round robin
  // -------------------
  val new_idx = WrapShift(VecInit(io.req_vec.map(_.valid)), sel_buf)
    .indexWhere(_ === true.B)
  val idx_map = WrapShift(VecInit(Seq.tabulate(numInputs)(_.U)), sel_buf)
  val sel_idx = idx_map(new_idx)

  val lock_valid = RegInit(false.B)
  when(io.req_arb.valid && !io.req_arb.ready) {
    lock_valid := true.B
  }.otherwise {
    lock_valid := false.B
  }

  io.sel_idx.valid := sel_buf_valid
  io.sel_idx.bits := sel_idx

  io.req_vec.foreach(_.nodeq())
  io.resp_vec.foreach(_.noenq())
  io.req_arb.noenq()
  io.resp_arb.nodeq()

  def select_input(): Unit = {
    when(sel_idx_valid) {
      assert(sel_idx < numInputs.U, "idx must less than %d".format(numInputs))
      assert(io.req_vec(sel_idx).valid, "in_req(idx) must be valid")

      val idx = Mux(lock_valid, sel_buf, sel_idx)

      io.req_arb <> io.req_vec(idx)

      when(!lock_valid) {
        sel_buf := sel_idx
      }

      when(io.req_arb.fire) {
        cur_max_priority := next_max_priority
        state := sWaitResp
      }.otherwise {
        state := sIdle
      }
    }.otherwise {
      state := sIdle
    }
  }

  switch(state) {
    is(sIdle) {
      select_input()
    }
    is(sWaitResp) {
      io.resp_vec(sel_buf) <> io.resp_arb
      when(io.resp_arb.fire && !io.flush) {
        select_input()
      }.elsewhen(io.flush) {
        state := sIdle
      }
    }
  }

  // --------------------------
  // formal
  // --------------------------
  if (formal) {
    assume(io.flush === false.B)
    when(io.flush) {
      for (i <- 0 until numInputs) {
        assert(io.req_vec(i).ready === false.B)
        assume(io.resp_vec(i).ready === false.B)
      }
      assume(io.req_arb.ready === false.B)
      assert(io.resp_arb.ready === false.B)
    }

    for (i <- 0 until numInputs) {
      when(FormalUtils.StreamShouldStable(io.req_vec(i))) {
        assume(io.req_vec(i).valid)
        assume(stable(io.req_vec(i).bits))
      }
      when(FormalUtils.StreamShouldStable(io.resp_vec(i))) {
        assert(io.resp_vec(i).valid)
        assert(stable(io.resp_vec(i).bits))
      }
    }
    when(FormalUtils.StreamShouldStable(io.req_arb)) {
      assert(io.req_arb.valid)
      assert(stable(io.req_arb.bits))
    }
    when(FormalUtils.StreamShouldStable(io.resp_arb)) {
      assume(io.resp_arb.valid)
      assume(stable(io.resp_arb.bits))
    }
  }
}

class ReqRespArbFormal
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Formal {
  "ReqRespArb" should "pass with assumption" in {
    verify(
      new ReqRespArbiter(4, UInt(32.W), UInt(32.W), formal = true),
      Seq(BoundedCheck(10))
    )
  }
}

object gen_DcacheArb_verilog extends App {
  GenVerilogHelper(new ReqRespArbiter(4, new LoadDcacheReq, new LoadDcacheResp))
}
