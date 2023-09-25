package leesum
import chisel3._
import chisel3.util.Decoupled

class LSUReq extends Bundle {
  val op_a = UInt(64.W)
  val op_b = UInt(64.W)
  val size = UInt(2.W)
  val store_data = UInt(64.W)
  val is_store = Bool()
  val trans_id = UInt(32.W)
}
class LSUResp extends Bundle {
  val trans_id = UInt(32.W)
  val wb_data = UInt(64.W)
  val exception = new ExceptionEntry(has_valid = true)
}

class LSU extends Module {
  val io = IO(new Bundle {
    val lsu_req = Flipped(Decoupled(new LSUReq))
    val flush = Input(Bool())

    // tlb interface
    val tlb_req = Decoupled(new TLBReq)
    val tlb_resp = Flipped(Decoupled(new TLBResp))

    // write-back interface
    val lsu_resp = Decoupled(new LSUResp)
  })

}
