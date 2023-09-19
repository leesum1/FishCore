package leesum

import chisel3._
import chisel3.util.{Decoupled, Enum, is, log2Ceil, switch}

class StoreQueueIn extends Bundle {
  val store_data = UInt(64.W)
  val paddr = UInt(64.W)
  // 0: 1 byte, 1: 2 bytes, 2: 4 bytes, 3: 8 bytes
  val size = UInt(2.W)
  val is_mmio = Bool()
}
class StoreFIFOEntry extends Bundle {
  val valid = Bool()
  val bits = new StoreQueueIn
}

class StoreQueue extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StoreQueueIn))
    val flush = Input(Bool())
    val store_commit = Input(Bool())
    val dcache_req = Decoupled(new StoreDcacheReq)
    val dcache_resp = Flipped(Decoupled(new StoreDcacheResp))
  })
  val speculate_store_queue_size = 4
  val commit_store_queue_size = 4

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
  def speculate_push(entry: StoreQueueIn): Unit = {
    speculate_store_fifo(speculate_push_ptr).bits := entry
    speculate_store_fifo(speculate_push_ptr).valid := true.B
    speculate_push_ptr := speculate_push_ptr + 1.U
    speculate_num_counter := speculate_num_counter + 1.U
  }
  def speculate_pop(): Unit = {
    speculate_store_fifo(speculate_pop_ptr).valid := false.B
    speculate_pop_ptr := speculate_pop_ptr + 1.U
    speculate_num_counter := speculate_num_counter - 1.U
  }
  def speculate_flush(): Unit = {
    speculate_push_ptr := 0.U
    speculate_pop_ptr := 0.U
    speculate_num_counter := 0.U
    0.until(speculate_store_queue_size).foreach { i =>
      speculate_store_fifo(i).valid := false.B
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

  def commit_push(entry: StoreQueueIn): Unit = {
    commit_store_fifo(commit_push_ptr).bits := entry
    commit_store_fifo(commit_push_ptr).valid := true.B
    commit_push_ptr := commit_push_ptr + 1.U
    commit_num_counter := commit_num_counter + 1.U
  }
  def commit_pop(): Unit = {
    commit_store_fifo(commit_pop_ptr).valid := false.B
    commit_pop_ptr := commit_pop_ptr + 1.U
    commit_num_counter := commit_num_counter - 1.U
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
  when(io.in.fire) {
    speculate_push(io.in.bits)
  }
  when(io.store_commit & !commit_fifo_full) {
    speculate_pop()
  }
  when(io.flush) {
    speculate_flush()
  }
  // -----------------------
  // commit store queue logic
  // -----------------------

  // when get a store commit signal, change speculate store queue to commit store queue
  when(io.store_commit & !commit_fifo_full) {
    commit_push(speculate_store_fifo(speculate_pop_ptr).bits)
  }
  val commit_pop_cond = WireInit(false.B)
  when(commit_pop_cond) {
    commit_pop()
  }
  // ----------------------------
  // Dcache logic & memory logic
  // ----------------------------
  val sIdle :: sWaitDcacheResp :: Nil = Enum(2)

  val state = RegInit(sIdle)
  io.dcache_req.valid := false.B
  io.dcache_req.bits := DontCare
  io.dcache_resp.ready := false.B

  def sen_dcache_store_req(
      paddr: UInt,
      data: UInt,
      size: UInt,
      is_mmio: Bool
  ): Unit = {
    io.dcache_req.valid := true.B
//    io.dcache_req.bits.mask := mask
    io.dcache_req.bits.paddr := paddr
    io.dcache_req.bits.wdata := GenWdataAlign(data, paddr)
    io.dcache_req.bits.wstrb := GenWstrb(paddr, size)
    io.dcache_req.bits.size := size
    io.dcache_req.bits.is_mmio := is_mmio
    when(io.dcache_req.fire) {
      state := sWaitDcacheResp
      commit_pop_cond := true.B
    }.otherwise {
      state := sIdle
    }
  }

  // TODO: back to back store
  switch(state) {
    is(sIdle) {
      when(!commit_fifo_empty) {
        val entry = commit_store_fifo(commit_pop_ptr)
        assert(entry.valid, "commit store queue must be valid")
        val paddr = entry.bits.paddr
        val data = entry.bits.store_data
        val size = entry.bits.size
        val is_mmio = entry.bits.is_mmio
        sen_dcache_store_req(paddr, data, size, is_mmio)
      }
    }
    is(sWaitDcacheResp) {
      io.dcache_resp.ready := true.B
      when(io.dcache_resp.fire) {
        state := sIdle
      }
    }
  }
}

object gen_store_queue_verilog extends App {
  GenVerilogHelper(new StoreQueue)
}
