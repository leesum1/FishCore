package leesum.Utils
import chisel3._
import chisel3.util.{RegEnable, ShiftRegister}
import leesum.GenVerilogHelper

object HoldRegister {

  /** This method creates a register that holds the input data when the trigger
    * signal is false. When the trigger signal is true, the register is updated
    * data_latency parameter.
    * @param trigger
    *   The trigger signal for the register.
    * @param data_in
    *   The input data for the register.
    * @param data_latency
    *   The latency of the The input data.
    * @return
    *   The output of the register.
    */
  def apply[T <: Data](trigger: Bool, data_in: T, data_latency: Int): T = {
    require(data_latency >= 0, "data_latency must be non-negative")
    if (data_latency > 0) {
      apply_with_latency(trigger, data_in, data_latency)
    } else {
      apply_no_latency(trigger, data_in)
    }
  }

  private def apply_with_latency[T <: Data](
      trigger: Bool,
      data_in: T,
      data_latency: Int
  ): T = {
    apply_no_latency(ShiftRegister(trigger, data_latency), data_in)
  }

  private def apply_no_latency[T <: Data](trigger: Bool, data_in: T): T = {
    val data_hold = RegEnable(data_in, trigger)
    Mux(trigger, data_in, data_hold)
  }
}

object gen_hold_register_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val trigger = Input(Bool())
      val data_in = Input(UInt(32.W))
      val data_out = Output(UInt(32.W))
    })
    io.data_out := HoldRegister(io.trigger, io.data_in, 2)
  })
}
