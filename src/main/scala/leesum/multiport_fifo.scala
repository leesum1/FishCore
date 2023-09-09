package leesum

import chisel3._
import chisel3.util.{PopCount, isPow2, log2Ceil}

class MultiportFIFO[T <: Data](
    val gen: T,
    val entries: Int,
    val num_pushports: Int,
    val num_popports: Int
) extends Module() {
  require(
    entries > 0,
    "MultiportFIFO must have non-zero number of entries"
  )
  require(
    isPow2(entries),
    "MultiportFIFO must have power-of-2 number of entries"
  )
  val genType = gen

  val io = IO(
    // 1. 只能一次 push num_writeports 个数据
    // 2. 可以一次 pop 小于等于 num_readports 个数据
    new Bundle {
      val push_data = Input(Vec(num_pushports, genType))
      val push_valid = Input(Vec(num_pushports, Bool()))

      val pop_data = Output(Vec(num_popports, genType))
      val pop_valid = Input(Vec(num_popports, Bool()))
      val flush = Input(Bool())
      val empty = Output(Bool())
      val full = Output(Bool())
      val free_entries = Output(UInt((log2Ceil(entries) + 1).W))
      val occupied_entries = Output(UInt((log2Ceil(entries) + 1).W))
    }
  )

  // only if previous push is valid, then current push can be valid
  // 0000 -> pass check
  // 1000 -> pass check
  // 1100 -> pass check
  // 1110 -> pass check
  // 1111 -> pass check
  // 0001 -> fail check
  // 0101 -> fail check
  def checkPopRspOrder(idx: Int): Bool = {
    idx match {
      case 0 => true.B
      case _ =>
        ((io.pop_valid(idx - 1) || !io.pop_valid(idx))) && checkPopRspOrder(
          idx - 1
        )
    }
  }
  // only if previous pop is valid, then current pop can be valid
  def checkPushValidOrder(idx: Int): Bool = {
    idx match {
      case 0 => true.B
      case _ =>
        ((io.push_valid(idx - 1) || !io.push_valid(
          idx
        ))) && checkPushValidOrder(
          idx - 1
        )
    }
  }

  assert(
    checkPopRspOrder(io.pop_valid.length - 1),
    "pop_valid must be ordered"
  )
  assert(
    checkPushValidOrder(io.push_valid.length - 1),
    "push_valid must be ordered"
  )

  ////////////////////////////
  /// internal memory
  ////////////////////////////
  val ram = Mem(entries, genType)
  val push_ptr = RegInit(0.U(log2Ceil(entries).W))
  val pop_ptr = RegInit(0.U(log2Ceil(entries).W))
  val num_counter = RegInit(0.U((log2Ceil(entries) + 1).W))

  num_counter := num_counter - PopCount(
    io.pop_valid
  ) + PopCount(
    io.push_valid
  )
  val empty = num_counter === 0.U
  val full = num_counter === entries.U
  val free_entries = entries.U - num_counter

  io.empty := empty
  io.full := full
  io.free_entries := free_entries
  io.occupied_entries := num_counter

  assert(
    PopCount(io.push_valid) <= free_entries,
    "push_valid must be less than or equal to free_entries"
  )
  assert(
    PopCount(io.pop_valid) <= num_counter,
    "pop_rsp must be less than or equal to num_counter"
  )

  // push logic
  0.until(num_pushports)
    .foreach(i => {
      when(io.push_valid(i)) {
        ram(push_ptr + i.U) := io.push_data(i)
      }
    })

  push_ptr := push_ptr + PopCount(io.push_valid)

  // pop logic
  0.until(num_popports)
    .foreach(i => {
      io.pop_data(i) := ram(pop_ptr + i.U)
    })
  pop_ptr := pop_ptr + PopCount(io.pop_valid)

  when(io.flush) {
    pop_ptr := 0.U
    push_ptr := 0.U
    num_counter := 0.U
  }
}

object gen_multiport_fifo_verilog extends App {
  GenVerilogHelper(new MultiportFIFO(UInt(32.W), 8, 4, 4))
}
