package leesum.fronten

import chisel3._
import chisel3.util.{BitPat, Decoupled, Enum, PriorityMux, ValidIO, is, switch}
import chiseltest.ChiselScalatestTester
import chiseltest.formal._
import leesum.Cache.{ICacheReq, ICacheResp}
import leesum.Utils.DecoderHelper
import leesum._
import org.scalatest.flatspec.AnyFlatSpec

class f2_f3_pipe_entry extends Bundle {
  val fetch_group = Vec(4, new INSTEntry)
  val exception = new ExceptionEntry()
  val exception_pc = UInt(64.W)

  def clear(): Unit = {
    fetch_group.foreach(_.clear())
    exception.clear()
    exception_pc := 0.U
  }
}

class IFUTop(rvc_en: Boolean = false, formal: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val pc_in = Flipped(Decoupled(UInt(64.W)))
    val icache_req = Decoupled(new ICacheReq)
    val icache_resp = Flipped(Decoupled(new ICacheResp))
    val inst_fifo_pop = Vec(2, Decoupled(new INSTEntry))
    val flush = Input(Bool())
    val f3_flush = Output(Bool())
    val f3_redirect_pc = Output(ValidIO(UInt(64.W)))
  })

  val sIdle :: sWaitResp :: sWaitPipe :: sFlushResp :: Nil = Enum(4)

  val state = RegInit(sIdle)

  val pc_f1_f2_buf = RegInit(0.U(64.W))

  val f2_f3_buf = Reg(new f2_f3_pipe_entry)

  val f3_flush_next = RegInit(false.B)
  when(f3_flush_next || io.flush) {
    f3_flush_next := false.B
  }
  io.f3_flush := f3_flush_next

//  val ifu_flush = io.flush || f3_flush_next

  when(reset.asBool) {
    f2_f3_buf.clear()
  }

  val f2_f3_pipe = Module(new PipeLine(new f2_f3_pipe_entry))
  f2_f3_pipe.io.flush := io.flush || f3_flush_next
  f2_f3_pipe.io.in.valid := false.B
  f2_f3_pipe.io.in.bits.clear()
  f2_f3_pipe.io.out.nodeq()

  val inst_realign = Module(new InstReAlign(rvc_en))
  inst_realign.io.flush := io.flush || f3_flush_next
  inst_realign.io.input.noenq()
  inst_realign.io.output.nodeq()

  io.icache_req.noenq()
  io.icache_resp.nodeq()
  io.pc_in.nodeq()
  io.f3_redirect_pc.valid := false.B
  io.f3_redirect_pc.bits := 0.U

  def send_icache_req() = {
    // TODO: check it about flush
    io.icache_req.valid := io.pc_in.valid && !f3_flush_next

    // 1. flush from commit
    // 2. flush from f3 branch prediction
    io.pc_in.ready := io.icache_req.ready && !io.flush && !f3_flush_next
    io.icache_req.bits.va := io.pc_in.bits

    when(io.icache_req.fire) {
      assert(!io.flush && !f3_flush_next)
      pc_f1_f2_buf := io.pc_in.bits
      state := sWaitResp
    }.otherwise {
      state := sIdle
    }
  }

  switch(state) {
    is(sIdle) {
      send_icache_req()
    }
    is(sWaitResp) {
      // TODO: branch prediction flush
      io.icache_resp.ready := true.B && !io.flush && !f3_flush_next
      when(io.icache_resp.fire) {
        assert(!io.flush && !f3_flush_next)
        inst_realign.io.input.valid := true.B
        inst_realign.io.output.ready := true.B
        inst_realign.io.input.bits.pc := pc_f1_f2_buf
        inst_realign.io.input.bits.payload := io.icache_resp.bits.payload

        f2_f3_pipe.io.in.valid := true.B
        f2_f3_pipe.io.in.bits.fetch_group := inst_realign.io.output.bits
        f2_f3_pipe.io.in.bits.exception := io.icache_resp.bits.exception

        // if last half is valid, put exception to the start of the instruction
        f2_f3_pipe.io.in.bits.exception_pc := Mux(
          inst_realign.io.last_half_valid,
          inst_realign.io.last_half_pc,
          pc_f1_f2_buf
        )

        f2_f3_buf.exception := io.icache_resp.bits.exception

        // if last half is valid, put exception to the start of the instruction
        f2_f3_buf.exception_pc := Mux(
          inst_realign.io.last_half_valid,
          inst_realign.io.last_half_pc,
          pc_f1_f2_buf
        )

        f2_f3_buf.fetch_group := inst_realign.io.output.bits

        when(f2_f3_pipe.io.in.fire) {
          send_icache_req()
        }.otherwise {
          state := sWaitPipe
        }
      }.elsewhen(io.flush) {
        state := sIdle
      }.elsewhen(f3_flush_next) {
        state := sFlushResp
      }
    }
    is(sWaitPipe) {
      f2_f3_pipe.io.in.valid := true.B && !io.flush && !f3_flush_next
      f2_f3_pipe.io.in.bits := f2_f3_buf
      when(f2_f3_pipe.io.in.fire) {
        send_icache_req()
      }.elsewhen(io.flush || f3_flush_next) {
        state := sIdle
      }
    }
    is(sFlushResp) {
      io.icache_resp.ready := true.B && !io.flush
      when(io.icache_resp.fire || io.flush) {
//        assert(!f3_flush_next, "f3_flush_next should be false")
        state := sIdle
      }
    }
  }

  // --------------------------
  // f3
  // --------------------------

  val inst_fifo = Module(new InstsFIFO)

  inst_fifo.io.flush := io.flush
  io.inst_fifo_pop <> inst_fifo.io.pop

  inst_fifo.io.push.valid := f2_f3_pipe.io.out.valid
  inst_fifo.io.push.bits.foreach(x => {
    x.clear()
  })
  f2_f3_pipe.io.out.ready := inst_fifo.io.push.ready

  when(f2_f3_pipe.io.out.fire && !io.flush && !f3_flush_next) {
    when(f2_f3_pipe.io.out.bits.exception.valid) {
      // if exception happens, put exception to the first entry
      inst_fifo.io.push.bits.foreach(x => {
        x.clear()
      })
      // override the first entry
      inst_fifo.io.push.bits(0).valid := true.B
      inst_fifo.io.push.bits(0).pc := f2_f3_pipe.io.out.bits.exception_pc
      inst_fifo.io.push.bits(0).exception := f2_f3_pipe.io.out.bits.exception
    }.otherwise {

      // TODO: do branch prediction here

      val static_bp = Seq.fill(4)(Module(new StaticBP))
      for (i <- 0 until 4) {
        static_bp(i).io.pc := f2_f3_pipe.io.out.bits.fetch_group(i).pc
        static_bp(i).io.inst := f2_f3_pipe.io.out.bits.fetch_group(i).inst
        static_bp(i).io.valid := f2_f3_pipe.io.out.bits.fetch_group(i).valid
        inst_fifo.io.push.bits(i) := f2_f3_pipe.io.out.bits.fetch_group(i)
      }

      val bp_res = VecInit(static_bp.map(_.io.bp))

      val taken_valid = bp_res.map(_.is_taken).reduce(_ || _)

      when(taken_valid) {
        f3_flush_next := true.B
      }

      val taken_seq = VecInit(bp_res.map(_.is_taken)).asUInt

      io.f3_redirect_pc.valid := taken_valid

      // select the first taken branch
      io.f3_redirect_pc.bits := PriorityMux(
        taken_seq,
        bp_res.map(_.predict_pc)
      )

      val bp_mask = VecInit(
        DecoderHelper(
          taken_seq,
          "b1111".U(4.W), // no branch
          Seq(
            BitPat("b???1") -> "b0001".U(4.W),
            BitPat("b??10") -> "b0011".U(4.W),
            BitPat("b?100") -> "b0111".U(4.W),
            BitPat("b1000") -> "b1111".U(4.W)
          )
        ).asBools
      )

      inst_fifo.io.push.bits := f2_f3_pipe.io.out.bits.fetch_group
      for (i <- 0 until 4) {
        // Override branch prediction
        inst_fifo.io.push.bits(i).bp := bp_res(i)
        // override valid
        inst_fifo.io.push.bits(i).valid := f2_f3_pipe.io.out.bits
          .fetch_group(
            i
          )
          .valid && bp_mask(i)
      }
    }
  }
  // --------------------------
  // formal
  // --------------------------

  when(RegNext(io.flush)) {
    assert(state === sIdle)
  }

  when(io.flush || f3_flush_next) {
    assume(io.icache_req.fire === false.B)
    assert(!io.pc_in.ready)
  }

  when(io.inst_fifo_pop.map(_.fire).reduce(_ || _)) {
    assume(CheckOrder(VecInit(io.inst_fifo_pop.map(_.fire))))
  }

  if (formal) {
    val f_flush = io.flush && past(io.flush)

    when(FormalUtils.StreamShouldStable(io.icache_resp) && f_flush) {
      assume(io.icache_resp.valid)
      assume(stable(io.icache_resp.bits))
    }
  }

}

class IFUTopFormal extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "IFUTop" should "pass with assumption" in {
    verify(
      new IFUTop(formal = true),
      Seq(BoundedCheck(10), CVC4EngineAnnotation)
    )
  }
}

object gen_ifu_verilog extends App {
  GenVerilogHelper(new IFUTop)
}
