package leesum

import Chisel.{Cat, switch}
import chisel3._
import chisel3.util.{Decoupled, Enum, Queue, is}
import chisel3.stage.ChiselStage

class INSTEmtry extends Bundle {
  val pc = UInt(32.W)
  val inst = UInt(32.W)
  val rvc = Bool()
  val valid = Bool()
}

class InstsFifoIO extends Bundle {
  val in = Flipped(Decoupled(new InstsItem))
  val out = Decoupled(new INSTEmtry)
}

class InstsFifo extends Module {
  val io = IO(new InstsFifoIO)

  val fifo: Queue[INSTEmtry] = Module(new Queue(new INSTEmtry, 16))

  val in_reg = RegInit(0.U.asTypeOf(new InstsItem))

  val sIdle :: sFirst :: sPush :: Nil = Enum(3)

  val state = RegInit(sIdle)

  val inst_idx = RegInit(0.U(4.W))

  val tmp_inst = RegInit(0.U.asTypeOf(new INSTEmtry))
  val tmp_valid = RegInit(false.B)

  val my_idx = find_first_valid_from(inst_idx)
  val is_idle = (state === sIdle)

  fifo.io.enq.valid := tmp_valid
  fifo.io.enq.bits := tmp_inst
  fifo.io.deq <> io.out

  def is_inst_valid(idx: UInt): Bool = {
    // require(idx >= 0 && idx < 4)s
    in_reg.insts_valid_mask(idx)
  }

  def is_inst_rvc(idx: UInt): Bool = {
    // require(idx >= 0 && idx < 4)
    in_reg.insts_rvc_mask(idx)
  }

  def get_inst(idx: UInt): UInt = {
    // require(idx >= 0 && idx < 4)s
    in_reg.insts(idx)
  }
  def get_inst_pc(idx: UInt): UInt = {
    // require(idx >= 0 && idx < 4)
    in_reg.insts_pc(idx)
  }

  def find_first_valid_from(idx: UInt): UInt = {

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

  def insts_fifo_enq(idx: UInt) = {
    tmp_valid := true.B
    inst_idx := my_idx + 1.U
    // data
    tmp_inst.pc := get_inst_pc(idx)
    tmp_inst.inst := get_inst(idx)
    tmp_inst.rvc := is_inst_rvc(idx)
    tmp_inst.valid := is_inst_valid(idx)
  }
  def insts_fifo_enq_clear() = {
    tmp_valid := false.B
    inst_idx := 0.U

    tmp_inst.valid := false.B
    tmp_inst.rvc := false.B
    tmp_inst.inst := 0.U
    tmp_inst.pc := 0.U
  }

  switch(state) {
    is(sIdle) {
      inst_idx := 0.U
      tmp_valid := false.B
      when(io.in.fire) {
        state := sFirst
      }
    }
    is(sFirst) {

      when(my_idx < 4.U) {

        insts_fifo_enq(my_idx)

        state := sPush

      }.otherwise {

        insts_fifo_enq_clear()

        when(io.in.fire) {
          state := sFirst
        }.otherwise { state := sIdle }
      }
    }
    is(sPush) {
      when(fifo.io.enq.fire) {
        when(my_idx < 4.U) {
          insts_fifo_enq(my_idx)
        }.otherwise {
          insts_fifo_enq_clear()
          when(io.in.fire) {
            state := sFirst
          }.otherwise { state := sIdle }
        }
      }
    }
  }

  // handshake
  when(io.in.fire) {
    in_reg := io.in.bits
  }

  val empty_logic = is_idle || (my_idx >= 4.U)
  io.in.ready := empty_logic

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
