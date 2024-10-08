package leesum.Cache

import chisel3._
import chisel3.util.Decoupled
import leesum._
import leesum.axi4.{AXIDef, AXIMasterIO, StreamFork}

/** DummyDCache,actually there is no cache, just convert load and store req to
  * axi memory req
  */
class CacheMMIOFSM extends Module {

  val axi_addr_width = 32
  val axi_data_width = 64

  val io = IO(new Bundle {
    val load_req = Flipped(Decoupled(new LoadDcacheReq))
    val load_resp = Decoupled(new LoadDcacheResp)
    val store_req = Flipped(Decoupled(new StoreDcacheReq))
    val store_resp = Decoupled(new StoreDcacheResp)

    val axi_mem = new AXIMasterIO(axi_addr_width, axi_data_width)
  })

  val axi_master = Wire(new AXIMasterIO(axi_addr_width, axi_data_width))

  axi_master.clear()

  io.axi_mem <> axi_master

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

  io.store_resp.bits.exception.valid := false.B
  io.store_resp.bits.exception.tval := 0.U
  io.store_resp.bits.exception.cause := ExceptionCause.store_access

  // --------------------------
  // assert
  // --------------------------

//  when(io.load_resp.fire) {
//    assert(load_req_fifo.valid === true.B, "load_req_fifo.valid must be true")
//  }

  when(io.load_req.fire) {
    assert(
      CheckAligned(io.load_req.bits.paddr, io.load_req.bits.size),
      "load address not aligned"
    )
  }
  when(io.store_req.fire) {
    assert(
      CheckAligned(io.store_req.bits.paddr, io.store_req.bits.size),
      "store address not aligned"
    )
  }
  when(axi_master.r.fire) {
    assert(axi_master.r.bits.id === 0.U, "id === 0.U")
    assert(
      axi_master.r.bits.resp === AXIDef.RESP_OKAY,
      "axi_master.r.bits.resp must be OKAY"
    )
    assert(
      axi_master.r.bits.last === true.B,
      "not support burst transfer"
    )
  }
  when(axi_master.b.fire) {
    assert(axi_master.b.bits.id === 0.U, "id === 0.U")
  }

}

object gen_CacheMMIOFSM_verilog extends App {
  GenVerilogHelper(new CacheMMIOFSM)
}
