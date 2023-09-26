package leesum
import chisel3._
import chisel3.util.{Arbiter, Decoupled}

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

    // dcache interface
    val dcache_load_req = Decoupled(new LoadDcacheReq)
    val dcache_load_resp = Flipped(Decoupled(new LoadDcacheResp))
    val dcache_store_req = Decoupled(new StoreDcacheReq)
    val dcache_store_resp = Flipped(Decoupled(new StoreDcacheResp))

    // commit interface
    val mmio_commit = Flipped(Decoupled(Bool()))
    val store_commit = Flipped(Decoupled(Bool()))

    // write-back interface
    val lsu_resp = Decoupled(new LSUResp)
  })

  val agu = Module(new AGU)
  val load_queue = Module(new LoadQueue)
  val store_queue = Module(new StoreQueue)

  val agu_write_back = Wire(Decoupled(new LSUResp))
  val load_write_back = Wire(Decoupled(new LSUResp))

  // flush
  agu.io.flush := io.flush
  load_queue.io.flush := io.flush
  store_queue.io.flush := io.flush
  // agu
  agu.io.in <> io.lsu_req
  // agu <> tlb
  agu.io.tlb_req <> io.tlb_req
  agu.io.tlb_resp <> io.tlb_resp
  // agu <> load queue
  agu.io.out.load_pipe <> load_queue.io.in
  // agu <> store queue
  agu.io.out.store_pipe <> store_queue.io.in
  // agu <> write-back
  agu_write_back.valid := agu.io.out.exception_pipe.valid
  agu_write_back.bits.exception := agu.io.out.exception_pipe.bits.exception
  agu_write_back.bits.trans_id := agu.io.out.exception_pipe.bits.trans_id
  agu_write_back.bits.wb_data := DontCare
  agu.io.out.exception_pipe.ready := agu_write_back.ready

  // load queue <> dcache
  load_queue.io.dcache_req <> io.dcache_load_req
  load_queue.io.dcache_resp <> io.dcache_load_resp
  // load queue <> commit
  load_queue.io.mmio_commit <> io.mmio_commit
  // load queue <> write-back
  load_write_back.valid := load_queue.io.load_wb.valid
  load_write_back.bits.exception.valid := false.B
  load_write_back.bits.exception.cause := DontCare
  load_write_back.bits.exception.tval := DontCare
  load_write_back.bits.wb_data := load_queue.io.load_wb.bits.rdata
  load_write_back.bits.trans_id := load_queue.io.load_wb.bits.tran_id
  load_queue.io.load_wb.ready := load_write_back.ready

  // store queue <> dcache
  store_queue.io.dcache_req <> io.dcache_store_req
  store_queue.io.dcache_resp <> io.dcache_store_resp
  // store queue <> commit
  store_queue.io.store_commit <> io.store_commit

  // ---------------------
  // select write-back
  // ---------------------
  val arb = Module(new Arbiter(new LSUResp, 2))
  // load write-back has higher priority
  arb.io.in(0) <> load_write_back
  arb.io.in(1) <> agu_write_back
  arb.io.out <> io.lsu_resp
}

object gen_lsu_verilog extends App {
  GenVerilogHelper(new LSU)
}
