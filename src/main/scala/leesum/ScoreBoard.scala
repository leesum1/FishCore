package leesum

import chisel3._
import chisel3.experimental.{DataMirror, requireIsChiselType}
import chisel3.stage.ChiselStage
import chisel3.util.{Counter, Decoupled, PopCount, isPow2, log2Ceil}

class ScoreBoard(
    val entries: Int,
    val num_pushports: Int,
    val num_popports: Int
)(implicit compileOptions: chisel3.CompileOptions)
    extends Module() {
  require(
    entries > 0,
    "ScoreBoard must have non-zero number of entries"
  )
  require(
    isPow2(entries),
    "ScoreBoard must have power-of-2 number of entries"
  )

  val genType = new ScoreBoardEntry()

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

      // writeback port
      // alu
      val fu_alu_valid = Input(Bool())
      val fu_alu_id = Input(UInt(log2Ceil(entries).W))
      val fu_alu_wb = Input(UInt(64.W))
      val fu_alu_exception =
        Input(new ExceptionEntry()) // use for fetch exception
      // branch
      val fu_branch_valid = Input(Bool())
      val fu_branch_id = Input(UInt(log2Ceil(entries).W))
      val fu_branch_wb = Input(UInt(64.W))
      val fu_branch_miss_predict = Input(Bool())
      val fu_branch_redirect_pc = Input(UInt(64.W))
      // load
      val fu_load_valid = Input(Bool())
      val fu_load_id = Input(UInt(log2Ceil(entries).W))
      val fu_load_wb = Input(UInt(64.W))
      val fu_load_io_space = Input(Bool()) // io space load,such as uart
      val fu_load_exception = Input(new ExceptionEntry())
      // store
      val fu_store_valid = Input(Bool())
      val fu_store_id = Input(UInt(log2Ceil(entries).W))
      val fu_store_exception = Input(new ExceptionEntry())
      // mul_div
      val fu_mul_div_valid = Input(Bool())
      val fu_mul_div_id = Input(UInt(log2Ceil(entries).W))
      val fu_mul_div_wb = Input(UInt(64.W))
      val fu_mul_div_exception = Input(new ExceptionEntry())

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

object gen_ScoreBoard_verilog extends App {
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new ScoreBoard(8, 4, 4),
      Array(
        "--target-dir",
        verilogDir,
        "--emission-options=disableMemRandomization,disableRegisterRandomization"
      )
    )
}
