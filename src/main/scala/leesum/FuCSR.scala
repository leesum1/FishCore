package leesum
import chisel3._
import chisel3.util.Decoupled

class CsrReq extends Bundle {
  val addr = UInt(12.W)
  val wdata = UInt(64.W)
  val wen = Bool()
}

class CsrResp extends Bundle {
  val rdata = UInt(64.W)
}

class FuCSR extends Module {
  val io = IO(new Bundle {
    val csr_req = Flipped(Decoupled(new CsrReq))
    val csr_resp = Decoupled(new CsrResp)

    // from commit stage, when commit a csr instruction, set csr_commit to true
    val csr_commit = Flipped(Decoupled(Bool()))
    val flush = Input(Bool())
  })
  val csr_fifo = new ValidFIFO(new CsrReq, 4, "csr_fifo")
}
