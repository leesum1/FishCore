package leesum
import chisel3._
import chisel3.util.{Decoupled, Enum, is, log2Ceil, switch}

/** This Module is a generic arbiter for request and response signals.The lowest
  * index input has the highest priority.
  * @param numInputs
  * @param reqType
  * @param respType
  * @tparam T
  * @tparam U
  */
class ReqRespArbiter[T <: Data, U <: Data](
    numInputs: Int,
    reqType: T,
    respType: U
) extends Module {

  override def desiredName: String = "Arb_%d_%s_%s".format(
    numInputs,
    reqType.toString,
    respType.toString
  )

  val io = IO(new Bundle {
    val req_vec = Vec(numInputs, Flipped(Decoupled(reqType)))
    val resp_vec = Vec(numInputs, Decoupled(respType))

    val req_arb = Decoupled(reqType)
    val resp_arb = Flipped(Decoupled(respType))
  })

  val sel_buf = RegInit(0.U(log2Ceil(numInputs).W))
  val sIdle :: sWaitResp :: Nil = Enum(2)

  val state = RegInit(sIdle)
  val sel_idx = VecInit(io.req_vec.map(_.valid)).indexWhere(_ === true.B)
  val sel_idx_valid = io.req_vec.map(_.valid).reduce(_ || _)

  io.req_vec.foreach(_.nodeq())
  io.resp_vec.foreach(_.noenq())
  io.req_arb.noenq()
  io.resp_arb.nodeq()

  def select_input(): Unit = {
    when(sel_idx_valid) {
      assert(sel_idx < numInputs.U, "idx must less than %d".format(numInputs))
      assert(io.req_vec(sel_idx).valid, "in_req(idx) must be valid")
      io.req_arb <> io.req_vec(sel_idx)
      when(io.req_arb.fire) {
        sel_buf := sel_idx
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
      when(io.resp_arb.fire) {
        select_input()
      }
    }
  }
}

object gen_DcacheArb_verilog extends App {
  GenVerilogHelper(new ReqRespArbiter(4, new LoadDcacheReq, new LoadDcacheResp))
}
