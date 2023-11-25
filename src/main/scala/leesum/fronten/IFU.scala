package leesum.fronten

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.{Cat, Decoupled, Enum, is, switch}
import chiseltest.ChiselScalatestTester
import chiseltest.formal.{
  BoundedCheck,
  CVC4EngineAnnotation,
  Formal,
  past,
  stable
}
import leesum.Cache.{ICacheReq, ICacheResp, ICacheTop}
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
  })

  val sIdle :: sWaitResp :: sWaitPipe :: Nil = Enum(3)

  val state = RegInit(sIdle)

  val pc_f1_f2_buf = RegInit(0.U(64.W))

  val f2_f3_buf = Reg(new f2_f3_pipe_entry)

  when(reset.asBool) {
    f2_f3_buf.clear()
  }

  val f2_f3_pipe = Module(new PipeLine(new f2_f3_pipe_entry))
  f2_f3_pipe.io.flush := io.flush
  f2_f3_pipe.io.in.valid := false.B
  f2_f3_pipe.io.in.bits.clear()
  f2_f3_pipe.io.out.nodeq()

  val inst_realign = Module(new InstReAlign(rvc_en))
  inst_realign.io.flush := io.flush
  inst_realign.io.input.noenq()
  inst_realign.io.output.nodeq()

  io.icache_req.noenq()
  io.icache_resp.nodeq()
  io.pc_in.nodeq()

  def send_icache_req() = {
    io.icache_req.valid := io.pc_in.valid
    io.pc_in.ready := io.icache_req.ready && !io.flush
    io.icache_req.bits.va := io.pc_in.bits

    when(io.icache_req.fire) {
      assert(!io.flush)
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
      io.icache_resp.ready := true.B && !io.flush
      when(io.icache_resp.fire) {
        assert(!io.flush)
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
      }
    }
    is(sWaitPipe) {
      f2_f3_pipe.io.in.valid := true.B
      f2_f3_pipe.io.in.bits := f2_f3_buf
      when(f2_f3_pipe.io.in.fire) {
        send_icache_req()
      }.elsewhen(io.flush) {
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
  f2_f3_pipe.io.out.ready := inst_fifo.io.push.ready

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
    inst_fifo.io.push.bits := f2_f3_pipe.io.out.bits.fetch_group
  }

  // --------------------------
  // formal
  // --------------------------

  when(RegNext(io.flush)) {
    assert(state === sIdle)
  }

  when(io.flush) {
    assume(io.icache_req.ready === false.B)
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
