package leesum

import Chisel.{Cat, switch}
import chisel3._
import chisel3.util.{Decoupled, Enum, Queue, is}
import chisel3.stage.ChiselStage

class INSTEntry extends Bundle {
  val pc = UInt(32.W)
  val inst = UInt(32.W)
  val rvc = Bool()
  val valid = Bool()

  def clear() = {
    pc := 0.U
    inst := 0.U
    rvc := false.B
    valid := false.B
  }
}

class InstsFifoIO extends Bundle {
  val in = Flipped(Decoupled(new InstsItem))
  val out = Decoupled(new INSTEntry)
  val flush = Input(Bool())
}

/** This module is used to convert a InstsItem to a stream of INSTEntry, Inside
  * this module, it will sequentially push valid INSTEntry from InstsItem to a
  * fifo. A InstsItem is a bundle of 4 INSTEntry. This module is a sequential
  * logic
  */
class InstsFifo extends Module {
  val io = IO(new InstsFifoIO)

  val inst_fifo: Queue[INSTEntry] = Module(
    new Queue(new INSTEntry, 16, hasFlush = true)
  )

  val insts_buffer = RegInit(0.U.asTypeOf(new InstsItem))

  // fsm
  val sIdle :: sFirst :: sPush :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val is_idle = (state === sIdle)

  // find first valid inst from inst_idx
  val current_idx = RegInit(0.U(4.W))
  val valid_inst_idx = find_first_valid_from(current_idx)
  // TODO make it configurable
  val idx_in_range = valid_inst_idx =/= 4.U

  val tmp_inst = RegInit(0.U.asTypeOf(new INSTEntry))
  val tmp_valid = RegInit(false.B)

  inst_fifo.io.enq.valid := tmp_valid
  inst_fifo.io.enq.bits := tmp_inst
  inst_fifo.io.deq <> io.out

  inst_fifo.flush := io.flush

  private def is_inst_valid(idx: UInt): Bool = {
    // require(idx >= 0 && idx < 4)s
    insts_buffer.insts_valid_mask(idx)
  }

  private def is_inst_rvc(idx: UInt): Bool = {
    // require(idx >= 0 && idx < 4)
    insts_buffer.insts_rvc_mask(idx)
  }

  private def get_inst(idx: UInt): UInt = {
    // require(idx >= 0 && idx < 4)s
    insts_buffer.insts(idx)
  }
  private def get_inst_pc(idx: UInt): UInt = {
    // require(idx >= 0 && idx < 4)
    insts_buffer.insts_pc(idx)
  }

  private def find_first_valid_from(idx: UInt): UInt = {

    // TODO make it configurable
    val valid_vec = VecInit(Seq.tabulate(4)(i => is_inst_valid(i.U)))

    // when indexWhere can't find a valid inst, it will return O.U
    // so we need to add a true.B(sentry) at the end of the masked_vec
    // to make sure indexWhere can always find a valid inst (maybe the sentry)
    val masked_vec = VecInit(
      Seq.tabulate(4)(i => valid_vec(i) && (i.U >= idx)) :+ true.B
    )
    val ret = masked_vec.indexWhere(_ === true.B)
    ret
  }

  private def insts_fifo_enq(idx: UInt) = {
    tmp_valid := true.B
    current_idx := valid_inst_idx + 1.U
    // data
    tmp_inst.pc := get_inst_pc(idx)
    tmp_inst.inst := get_inst(idx)
    tmp_inst.rvc := is_inst_rvc(idx)
    tmp_inst.valid := is_inst_valid(idx)
  }
  private def insts_fifo_enq_clear() = {
    tmp_valid := false.B
    current_idx := 0.U

    tmp_inst.valid := false.B
    tmp_inst.rvc := false.B
    tmp_inst.inst := 0.U
    tmp_inst.pc := 0.U
  }

  switch(state) {
    is(sIdle) {
      current_idx := 0.U
      tmp_valid := false.B
      when(io.in.fire && !io.flush) {
        state := sFirst
      }
    }
    is(sFirst) {

      when(idx_in_range && !io.flush) {
        insts_fifo_enq(valid_inst_idx)
        state := sPush
      }.otherwise {
        insts_fifo_enq_clear()
        when(io.in.fire && !io.flush) {
          state := sFirst
        }.otherwise { state := sIdle }
      }
    }
    is(sPush) {
      when(inst_fifo.io.enq.fire && !io.flush) {
        when(idx_in_range && !io.flush) {
          insts_fifo_enq(valid_inst_idx)
        }.otherwise {
          insts_fifo_enq_clear()
          when(io.in.fire && !io.flush) {
            state := sFirst
          }.otherwise { state := sIdle }
        }
      }
    }
  }

  // handshake
  when(io.in.fire) {
    insts_buffer := io.in.bits
  }

  val empty_logic = is_idle || (!idx_in_range)
  io.in.ready := empty_logic & inst_fifo.io.enq.ready & !io.flush
}

object gen_fifo_verilog extends App {
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new InstsFifo(),
      Array("--target-dir", verilogDir)
    )
}
