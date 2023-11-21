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

  val issue_fifo = new MultiPortFIFOBase(
    new ScoreBoardEntry,
    8,
    2,
    2,
    use_mem = false,
    with_valid = false
  )

  issue_fifo.push_pop_flush_cond(
    push_cond = VecInit(Seq.fill(2)(io.push.fire)),
    pop_cond = io.pop_valid,
    entry = io.push.bits,
    flush_cond = io.flush
  )
  io.push.ready := issue_fifo.free_entries >= 2.U

  val pop_peek = issue_fifo.peek()

  for (i <- 0 until 2) {
    io.pop_data(i) := pop_peek(i).bits
  }

  io.occupied_entries := issue_fifo.occupied_entries
}

object gen_IssueFIFO_verilog extends App {
  GenVerilogHelper(new IssueFIFO)
}
