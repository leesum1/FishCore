package leesum
import chisel3._
import chisel3.util.Decoupled

class LSUReq extends AGUReq {}
class LSUResp extends Bundle {
  val trans_id = UInt(32.W)
  val wb_data = UInt(64.W)

  def wb_data_valid = true.B
}

class LSU(
    // ADDR_START, ADDR_END, mmio
    addr_map: Seq[(Long, Long, Boolean)] = Seq(
      (
        0,
        0xffffffffffffffffL,
        false
      ) // default addr map, 0x0 -> 0xffffffffffffffff, mmio = false
    )
) extends Module {
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
    val amo_commit = Flipped(Decoupled(Bool()))

    // write-back interface TODO: Add a arbiter
    val lsu_resp = Decoupled(new LSUResp)
    val amo_writeback = Decoupled(new LSUResp)
    val agu_writeback = Decoupled(new AGUWriteBack)
  })

  val agu = Module(new AGU(addr_map))
  val load_queue = Module(new LoadQueue)
  val store_queue = Module(new StoreQueue)
  val amo_queue = Module(new AMOQueue)

  val load_arb = Module(
    new ReqRespArbiter(2, new LoadDcacheReq, new LoadDcacheResp)
  )

  val store_arb = Module(
    new ReqRespArbiter(2, new StoreDcacheReq, new StoreDcacheResp)
  )

  load_arb.io.flush := false.B
  store_arb.io.flush := false.B
  load_arb.io.req_arb <> io.dcache_load_req
  load_arb.io.resp_arb <> io.dcache_load_resp
  store_arb.io.req_arb <> io.dcache_store_req
  store_arb.io.resp_arb <> io.dcache_store_resp

  val load_write_back = Wire(Decoupled(new LSUResp))

  // flush
  agu.io.flush := io.flush
  amo_queue.io.flush := io.flush
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
  agu.io.store_bypass <> store_queue.io.store_bypass
  // agu <> amo queue
  agu.io.out.amo_pipe <> amo_queue.io.in

  // agu <> write-back
  io.agu_writeback <> agu.io.out.agu_pipe

  // amo queue <> store queue
  amo_queue.io.store_queue_empty := store_queue.io.st_queue_empty

  // load queue <> commit
  load_queue.io.mmio_commit <> io.mmio_commit
  // load queue <> write-back
  load_write_back.valid := load_queue.io.load_wb.valid
  load_write_back.bits.wb_data := load_queue.io.load_wb.bits.rdata
  load_write_back.bits.trans_id := load_queue.io.load_wb.bits.tran_id
  load_queue.io.load_wb.ready := load_write_back.ready

  // store queue <> dcache store
  store_queue.io.dcache_req <> store_arb.io.req_vec(1)
  store_queue.io.dcache_resp <> store_arb.io.resp_vec(1)

  // load queue <> dcache load
  load_queue.io.dcache_req <> load_arb.io.req_vec(1)
  load_queue.io.dcache_resp <> load_arb.io.resp_vec(1)

  // amo queue <> dcache load
  amo_queue.io.load_req <> load_arb.io.req_vec(0)
  amo_queue.io.load_resp <> load_arb.io.resp_vec(0)

  // amo queue <> dcache store
  amo_queue.io.store_req <> store_arb.io.req_vec(0)
  amo_queue.io.store_resp <> store_arb.io.resp_vec(0)

  // store queue <> commit
  store_queue.io.store_commit <> io.store_commit
  // agu queue <> commit
  amo_queue.io.amo_commit <> io.amo_commit

  // load queue <> write-back
  io.lsu_resp <> load_write_back
  // amo queue <> write-back
  io.amo_writeback <> amo_queue.io.amo_writeback
}

object gen_lsu_verilog extends App {
  GenVerilogHelper(new LSU)
}
