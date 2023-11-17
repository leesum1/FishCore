package leesum.axi4
import chisel3._
import leesum.ReqRespArbiter

class AxiReadArbiter extends Module {

  val io = IO(new Bundle {
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

  axi_r_arb.io.flush := false.B
  axi_r_arb.io.req_vec <> io.in.map(_.ar)
  axi_r_arb.io.resp_vec <> io.in.map(_.r)
  io.out.ar <> axi_r_arb.io.req_arb
  io.out.r <> axi_r_arb.io.resp_arb

}
