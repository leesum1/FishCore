package leesum
import chisel3._
import chisel3.util.Decoupled
import leesum.axi4.{AXI4Memory, AXIDef, AXIMasterIO, StreamFork}

class DummyDCache extends Module {
  val io = IO(new Bundle {
    val load_req = Flipped(Decoupled(new LoadDcacheReq))
    val load_resp = Decoupled(new LoadDcacheResp)
    val store_req = Flipped(Decoupled(new StoreDcacheReq))
    val store_resp = Decoupled(new StoreDcacheResp)
    val flush = Input(Bool())
  })
  val axi_addr_width = 12
  val axi_data_width = 64

  val axi_master = Wire(new AXIMasterIO(axi_addr_width, axi_data_width))

  val axi_mem = Module(new AXI4Memory)

  axi_mem.io <> axi_master

  axi_master.clear()
  // --------------------------
  // load to axi
  // --------------------------
  // ar
  axi_master.ar.valid := io.load_req.valid
  io.load_req.ready := axi_master.ar.ready
  axi_master.ar.bits.addr := io.load_req.bits.paddr
  axi_master.ar.bits.size := DcacheSize2AxiSize(io.load_req.bits.size)
  axi_master.ar.bits.burst := AXIDef.BURST_INCR
  axi_master.ar.bits.len := 0.U
  axi_master.ar.bits.id := 0.U

  // r
  io.load_resp.valid := axi_master.r.valid
  axi_master.r.ready := io.load_resp.ready
  io.load_resp.bits.data := axi_master.r.bits.data

  when(axi_master.r.fire) {
    assert(axi_master.r.bits.id === 0.U, "axi_master.r.bits.id === 0.U")
    assert(
      axi_master.r.bits.resp === AXIDef.RESP_OKAY,
      "axi_master.r.bits.resp === 0.U"
    )
    assert(
      axi_master.r.bits.last === true.B,
      "axi_master.r.bits.last === true.B"
    )
  }

  io.load_resp.bits.exception.valid := false.B
  io.load_resp.bits.exception.tval := 0.U
  io.load_resp.bits.exception.cause := ExceptionCause.load_access
  // --------------------------
  // store to axi
  // --------------------------
  val store_req_fork = Module(new StreamFork(new StoreDcacheReq, 2))
  store_req_fork.io.input <> io.store_req

  // aw channel
  axi_master.aw.valid := store_req_fork.io.outputs(0).valid
  store_req_fork.io.outputs(0).ready := axi_master.aw.ready
  axi_master.aw.bits.addr := store_req_fork.io.outputs(0).bits.paddr
  axi_master.aw.bits.size := DcacheSize2AxiSize(
    store_req_fork.io.outputs(0).bits.size
  )
  axi_master.aw.bits.burst := AXIDef.BURST_INCR
  axi_master.aw.bits.len := 0.U
  axi_master.aw.bits.id := 0.U
  // w channel
  axi_master.w.valid := store_req_fork.io.outputs(1).valid
  store_req_fork.io.outputs(1).ready := axi_master.w.ready
  axi_master.w.bits.data := store_req_fork.io.outputs(1).bits.wdata
  axi_master.w.bits.strb := store_req_fork.io.outputs(1).bits.wstrb
  axi_master.w.bits.last := true.B
  axi_master.w.bits.user := 0.U
  // b channel
  io.store_resp.valid := axi_master.b.valid
  axi_master.b.ready := io.store_resp.ready
  when(axi_master.b.fire) {
    assert(axi_master.b.bits.id === 0.U, "axi_master.b.bits.id === 0.U")
  }

  io.store_resp.bits.exception.valid := false.B
  io.store_resp.bits.exception.tval := 0.U
  io.store_resp.bits.exception.cause := ExceptionCause.store_access

}

object gen_dummy_dcache_verilog extends App {
  GenVerilogHelper(new DummyDCache)
}
