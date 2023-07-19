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

  val fifo: Queue[INSTEmtry] = Module(new Queue(new INSTEmtry, 8))

  val in_reg = RegInit(0.U.asTypeOf(new InstsItem))
  val in_reg_empty = RegInit(true.B)

  def is_inst_valid(idx: UInt): Bool = {
    // require(idx >= 0 && idx < 4)s
    in_reg.insts_valid_mask(idx) & !in_reg_empty
  }

  def is_inst_rvc(idx: UInt): Bool = {
    // require(idx >= 0 && idx < 4)
    in_reg.insts_rvc_mask(idx) & !in_reg_empty
  }

  def get_inst(idx: UInt): UInt = {
    // require(idx >= 0 && idx < 4)s
    in_reg.insts(idx)
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

  val sIdle :: sFirst :: sPush :: Nil = Enum(3)

  val state = RegInit(sIdle)
  val inst_idx = RegInit(0.U(4.W))

  val tmp_inst = RegInit(0.U.asTypeOf(new INSTEmtry))
  val tmp_valid = RegInit(false.B)

  fifo.io.enq.valid := tmp_valid
  fifo.io.enq.bits := tmp_inst
  fifo.io.deq <> io.out

  switch(state) {
    is(sIdle) {
      inst_idx := 0.U
      fifo.io.enq.valid := false.B
      when(io.in.fire) {
        state := sFirst
      }
    }
    is(sFirst) {
      val my_idx = find_first_valid_from(inst_idx)

      when(my_idx < 4.U) {
        tmp_valid := true.B
        // data
        tmp_inst.pc := in_reg.insts_pc(my_idx)
        tmp_inst.inst := in_reg.insts(my_idx)
        tmp_inst.rvc := in_reg.insts_rvc_mask(my_idx)
        tmp_inst.valid := in_reg.insts_valid_mask(my_idx)

        inst_idx := my_idx + 1.U
        state := sPush

      }.otherwise {
        in_reg_empty := true.B

        tmp_valid := false.B
        tmp_inst.valid := false.B
        tmp_inst.rvc := 0.U
        tmp_inst.inst := 0.U
        tmp_inst.pc := 0.U

        inst_idx := 0.U
        state := sIdle
      }
    }
    is(sPush) {

      when(fifo.io.enq.valid && fifo.io.enq.ready) {
        val my_idx = find_first_valid_from(inst_idx)
        when(my_idx < 4.U) {
          tmp_valid := true.B
          // data
          tmp_inst.pc := in_reg.insts_pc(my_idx)
          tmp_inst.inst := in_reg.insts(my_idx)
          tmp_inst.rvc := in_reg.insts_rvc_mask(my_idx)
          tmp_inst.valid := in_reg.insts_valid_mask(my_idx)

          inst_idx := my_idx + 1.U
        }.otherwise {
          in_reg_empty := true.B

          tmp_valid := false.B
          tmp_inst.valid := false.B
          tmp_inst.rvc := false.B
          tmp_inst.inst := 0.U
          tmp_inst.pc := 0.U

          state := sIdle
          inst_idx := 0.U
        }
      }

    }
  }

  // handshake
  when(io.in.fire) {
    in_reg := io.in.bits
    in_reg_empty := false.B
  }

  io.in.ready := in_reg_empty

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
