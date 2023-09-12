package leesum
import chisel3._
import chisel3.util._
class IssueFIFO extends Module {
  val io = IO(new Bundle {
    val push = Flipped(Decoupled(Vec(2, new ScoreBoardEntry)))
    val pop_valid = Input(Vec(2, Bool()))
    val pop_data = Output(Vec(2, new ScoreBoardEntry))
    val occupied_entries = Output(UInt((log2Ceil(8) + 1).W))
    val flush = Input(Bool())
  })

  val issue_fifo = Module(new MultiportFIFO(new ScoreBoardEntry, 8, 2, 2))

  io.push.ready := issue_fifo.io.free_entries >= 2.U
  issue_fifo.io.push_valid := VecInit(Seq.fill(2)(io.push.fire))
  issue_fifo.io.push_data := io.push.bits

  io.pop_data := issue_fifo.io.pop_data
  issue_fifo.io.pop_valid := io.pop_valid
  io.occupied_entries := issue_fifo.io.occupied_entries
  issue_fifo.io.flush := io.flush
}

object gen_IssueFIFO_verilog extends App {
  GenVerilogHelper(new IssueFIFO)
}
