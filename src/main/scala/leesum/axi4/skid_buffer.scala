package leesum.axi4
import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Enum, is, switch}
import circt.stage.ChiselStage
import leesum.GenVerilogHelper

class skid_buffer[T <: Data](gen: T, CUT_VALID: Boolean, CUT_READY: Boolean)
    extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(gen))
    val out = Decoupled(gen)
  })

  (CUT_VALID, CUT_READY) match {
    case (true, true) => {
      val cut_valid = Module(new skid_buffer_cut_valid(gen))
      val cut_ready = Module(new skid_buffer_cut_ready(gen))
      cut_valid.io.in <> io.in
      cut_ready.io.in <> cut_valid.io.out
      cut_ready.io.out <> io.out
    }
    case (true, false) => {
      val cut_valid = Module(new skid_buffer_cut_valid(gen))
      cut_valid.io.in <> io.in
      cut_valid.io.out <> io.out
    }
    case (false, true) => {
      val cut_ready = Module(new skid_buffer_cut_ready(gen))
      cut_ready.io.in <> io.in
      cut_ready.io.out <> io.out
    }
    case (false, false) => {
      io.out <> io.in
    }
  }

}

private class skid_buffer_cut_valid[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(gen))
    val out = Decoupled(gen)
  })

  val buffer_T = RegInit(0.U.asTypeOf(gen))

  val sEmpty :: sBusy :: Nil = Enum(2)

  val state = RegInit(sEmpty)

  when(io.in.fire) {
    buffer_T := io.in.bits
  }

  io.out.valid := false.B
  io.in.ready := false.B
  switch(state) {
    is(sEmpty) {
      // buffer is empty, ready to receive data
      io.in.ready := true.B
      when(io.in.fire) {
        state := sBusy
      }
    }
    is(sBusy) {
      io.out.valid := true.B
      io.in.ready := io.out.ready
      // send data to out,and receive data from in
      when(io.out.fire && io.in.fire) {
        state := sBusy
      } // send data to out, but not receive data from in
        .elsewhen(io.out.fire && !io.in.fire) {
          state := sEmpty
        }
        // not send data to out, and not receive data from in
        .otherwise {
          state := sBusy
        }
    }
  }
  io.out.bits := buffer_T
}

private class skid_buffer_cut_ready[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(gen))
    val out = Decoupled(gen)
  })

  val buffer_T = RegInit(0.U.asTypeOf(gen))

  val sEmpty :: sBusy :: Nil = Enum(2)

  val state = RegInit(sEmpty)

  io.out.valid := false.B
  io.in.ready := false.B
  io.out.bits := DontCare
  switch(state) {
    is(sEmpty) {
      // buffer is empty, ready to receive data from Master
      io.in.ready := true.B
      // when buffer is empty, and Master send a valid data request
      // 1. if Slave is ready to receive data, send data to out
      // 2. if Slave is not ready to receive data, buffer is busy
      when(io.in.fire) {
        io.out.valid := true.B
        when(io.out.fire) {
          io.out.bits := io.in.bits
          state := sEmpty
        }.otherwise {
          buffer_T := io.in.bits
          state := sBusy
        }
      }
    }
    is(sBusy) {
      // buffer is busy, ready to send data to Slave
      io.out.valid := true.B
      io.out.bits := buffer_T
      // when Slave ready to receive data, send data to out
      // and set buffer to empty
      when(io.out.fire) {
        state := sEmpty
      }
    }
  }
}

object skid_buffer {
  def apply[T <: Data](
      in: DecoupledIO[T],
      out: DecoupledIO[T],
      CUT_VALID: Boolean,
      CUT_READY: Boolean
  ): Unit = {
    val buffer = Module(
      new skid_buffer(in.bits.cloneType, CUT_VALID, CUT_READY)
    )
    buffer.io.in.valid := in.valid
    in.ready := buffer.io.in.ready
    buffer.io.in.bits := in.bits

    out.valid := buffer.io.out.valid
    buffer.io.out.ready := out.ready
    out.bits := buffer.io.out.bits
  }
}

object gen_skid_buff_verilog extends App {
  GenVerilogHelper(new skid_buffer(UInt(32.W), true, false))
}
