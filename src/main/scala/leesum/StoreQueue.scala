package leesum

import chisel3._
import chisel3.util.{
  Decoupled,
  Enum,
  Mux1H,
  PopCount,
  is,
  isPow2,
  log2Ceil,
  switch
}

class StoreQueueIn extends Bundle {
  val wdata = UInt(64.W)
  val wstrb = UInt(8.W)
  val paddr = UInt(64.W)
  // 0: 1 byte, 1: 2 bytes, 2: 4 bytes, 3: 8 bytes
  val size = UInt(2.W)
  val is_mmio = Bool()
  val trans_id = UInt(32.W)
}
class StoreFIFOEntry extends Bundle {
  val valid = Bool()
  val bits = new StoreQueueIn
}

class StoreBypassData extends Bundle {
  val valid = Bool()
  val wdata = UInt(64.W)
  val wstrb = UInt(8.W)
  val is_mmio = Bool()
}

class StoreBypassIO extends Bundle {
  val valid = Input(Bool())
  val paddr = Input(UInt(64.W))
  val data = Output(new StoreBypassData)
}

class StoreQueue(
    speculate_store_queue_size: Int = 4,
    commit_store_queue_size: Int = 4
) extends Module {
  require(
    isPow2(speculate_store_queue_size) && isPow2(commit_store_queue_size),
    "speculate_store_queue_size and commit_store_queue_size must be power of 2"
  )

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StoreQueueIn))
    val flush = Input(Bool())
    val store_commit = Flipped(Decoupled(Bool()))
    val dcache_req = Decoupled(new StoreDcacheReq)
    val dcache_resp = Flipped(Decoupled(new StoreDcacheResp))
    val store_bypass = new StoreBypassIO
  })

  // --------------------------
  // speculate store queue
  // --------------------------
  val speculate_store_fifo =
    Mem(speculate_store_queue_size, new StoreFIFOEntry)
  val speculate_push_ptr = RegInit(
    0.U(log2Ceil(speculate_store_queue_size).W)
  )
  val speculate_pop_ptr = RegInit(0.U(log2Ceil(speculate_store_queue_size).W))
  val speculate_num_counter = RegInit(
    0.U((log2Ceil(speculate_store_queue_size) + 1).W)
  )
  val speculate_fifo_empty = speculate_num_counter === 0.U
  val speculate_fifo_full =
    speculate_num_counter === speculate_store_queue_size.U

  def speculate_push_pop_cond(
      push_cond: Bool,
      pop_cond: Bool,
      entry: StoreQueueIn
  ): Unit = {
    when(push_cond) {
      speculate_store_fifo(speculate_push_ptr).bits := entry
      speculate_store_fifo(speculate_push_ptr).valid := true.B

      speculate_push_ptr := speculate_push_ptr + 1.U
      speculate_num_counter := speculate_num_counter + 1.U
    }
    when(pop_cond) {
      speculate_store_fifo(speculate_pop_ptr).valid := false.B
      speculate_pop_ptr := speculate_pop_ptr + 1.U
      speculate_num_counter := speculate_num_counter - 1.U
    }
    when(push_cond && pop_cond) {
      speculate_num_counter := speculate_num_counter
    }
  }
  def speculate_flush(flush_cond: Bool): Unit = {
    when(flush_cond) {
      speculate_push_ptr := 0.U
      speculate_pop_ptr := 0.U
      speculate_num_counter := 0.U
      0.until(speculate_store_queue_size).foreach { i =>
        speculate_store_fifo(i).valid := false.B
      }
    }
  }

  // --------------------------
  // commit store queue
  // --------------------------
  val commit_store_fifo =
    Mem(commit_store_queue_size, new StoreFIFOEntry)

  val commit_push_ptr = RegInit(
    0.U(log2Ceil(commit_store_queue_size).W)
  )
  val commit_pop_ptr = RegInit(0.U(log2Ceil(commit_store_queue_size).W))
  val commit_num_counter = RegInit(
    0.U((log2Ceil(commit_store_queue_size) + 1).W)
  )

  val commit_fifo_empty = commit_num_counter === 0.U
  val commit_fifo_full =
    commit_num_counter === commit_store_queue_size.U

  def commit_push_pop_cond(
      push_cond: Bool,
      pop_cond: Bool,
      entry: StoreQueueIn
  ): Unit = {
    when(push_cond) {
      commit_store_fifo(commit_push_ptr).bits := entry
      commit_store_fifo(commit_push_ptr).valid := true.B
      commit_push_ptr := commit_push_ptr + 1.U
      commit_num_counter := commit_num_counter + 1.U
    }
    when(pop_cond) {
      commit_store_fifo(commit_pop_ptr).valid := false.B
      commit_pop_ptr := commit_pop_ptr + 1.U
      commit_num_counter := commit_num_counter - 1.U
    }
    when(push_cond && pop_cond) {
      commit_num_counter := commit_num_counter
    }
  }

  def commit_flush(): Unit = {
    commit_push_ptr := 0.U
    commit_pop_ptr := 0.U
    commit_num_counter := 0.U
    0.until(commit_store_queue_size).foreach { i =>
      commit_store_fifo(i).valid := false.B
    }
  }
  // --------------------------
  // speculate store queue logic
  // --------------------------
  io.in.ready := !speculate_fifo_full

  speculate_push_pop_cond(
    io.in.fire,
    io.store_commit.fire,
    io.in.bits
  )
  speculate_flush(io.flush)

  // -----------------------
  // commit store queue logic
  // -----------------------
  val commit_pop_cond = WireInit(false.B)

  // when get a store commit signal, transfer speculate store queue to commit store queue
  commit_push_pop_cond(
    io.store_commit.fire && !io.flush,
    commit_pop_cond,
    speculate_store_fifo.read(speculate_pop_ptr).bits
  )

  // ----------------------------
  // Dcache logic & memory logic
  // ----------------------------
  val sIdle :: sWaitDcacheResp :: Nil = Enum(2)

  val state = RegInit(sIdle)
  io.dcache_req.valid := false.B
  io.dcache_req.bits := DontCare
  io.dcache_resp.ready := false.B
  commit_pop_cond := false.B

  def send_dcache_store_req() = {
    when(!commit_fifo_empty) {
      val entry = commit_store_fifo(commit_pop_ptr)
      assert(entry.valid, "commit store queue must be valid")

      io.dcache_req.valid := true.B
      io.dcache_req.bits.paddr := entry.bits.paddr
      io.dcache_req.bits.wdata := entry.bits.wdata
      io.dcache_req.bits.wstrb := entry.bits.wstrb
      io.dcache_req.bits.size := entry.bits.size
      io.dcache_req.bits.is_mmio := entry.bits.is_mmio

      when(io.dcache_req.fire) {
        state := sWaitDcacheResp
        commit_pop_cond := true.B
      }.otherwise {
        state := sIdle
      }
    }.otherwise {
      state := sIdle
    }
  }

  switch(state) {
    is(sIdle) {
      send_dcache_store_req()
    }
    is(sWaitDcacheResp) {
      io.dcache_resp.ready := true.B
      when(io.dcache_resp.fire) {
        // back to back
        send_dcache_store_req()
      }
    }
  }
  // ----------------------
  // store commit logic
  // ----------------------
  io.store_commit.ready := !speculate_fifo_empty && !commit_fifo_full

  // ----------------------
  // store bypass logic
  // ----------------------

  def addr_match(addr1: UInt, addr2: UInt): Bool = {
    // TODO: need optimization?
    addr1(63, 3) === addr2(63, 3)
  }

  val all_fifo: Seq[StoreFIFOEntry] =
    0.until(speculate_store_queue_size).indices.map { i =>
      speculate_store_fifo(i)
    } ++ 0.until(commit_store_queue_size).indices.map { i =>
      commit_store_fifo(i)
    }
  val addr_match_mask = all_fifo.map { entry =>
    addr_match(
      entry.bits.paddr,
      io.store_bypass.paddr
    ) && entry.valid && io.store_bypass.valid
  }

  val combined_seq = all_fifo.map { entry =>
    val fwd_data = Wire(new StoreBypassData)
    fwd_data.wdata := entry.bits.wdata
    fwd_data.wstrb := entry.bits.wstrb
    fwd_data.is_mmio := entry.bits.is_mmio
    // first set valid to false, then set valid to true if addr match
    fwd_data.valid := false.B
    fwd_data
  }

  val bypass_select = Mux1H(addr_match_mask, combined_seq)
  val bypass_valid = addr_match_mask.reduce(_ || _)
  io.store_bypass.data := bypass_select
  // override valid signal, if addr match
  io.store_bypass.data.valid := bypass_valid

  // ----------------------
  // assert
  // ----------------------

  when(io.dcache_resp.fire) {
    assert(
      io.dcache_resp.bits.exception.valid === false.B,
      "not handle store_access exception now"
    )
  }
  assert(
    PopCount(addr_match_mask) <= 1.U,
    "store bypass should not match more than one address"
  )
  when(io.in.fire) {
    assert(!speculate_fifo_full, "speculate store queue must not be full")
  }
  when(io.store_commit.fire) {
    assert(
      !commit_fifo_full && !speculate_fifo_empty,
      "commit store queue must not be full, speculate store queue must not be empty"
    )
  }
  assert(
    !(io.flush && io.store_commit.valid),
    "flush and store_commit should not be valid at the same time"
  )

}

object gen_store_queue_verilog extends App {
  GenVerilogHelper(new StoreQueue)
}
