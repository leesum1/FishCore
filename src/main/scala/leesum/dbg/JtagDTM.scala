package leesum.dbg
import chisel3._
import chisel3.util.{DecoupledIO, _}
import leesum.GenVerilogHelper

class JtagIO(as_master: Boolean) extends Bundle {
  val tck = if (as_master) Output(Bool()) else Input(Bool())
  val tms = if (as_master) Output(Bool()) else Input(Bool())
  val tdi = if (as_master) Output(Bool()) else Input(Bool())
  val tdo = if (as_master) Input(Bool()) else Output(Bool())

  def clear(): Unit = {
    if (as_master) {
      tck := false.B
      tms := false.B
      tdi := false.B
    } else {
      tdo := false.B
    }
  }
}

object JtagState extends ChiselEnum {
  val TestLogicReset, RunTestIdle, SelectDrScan, CaptureDr, ShiftDr, Exit1Dr,
      PauseDr, Exit2Dr, UpdateDr, SelectIrScan, CaptureIr, ShiftIr, Exit1Ir,
      PauseIr, Exit2Ir, UpdateIr = Value

  def get_next_state(cur: JtagState.Type, tms_i: Bool): JtagState.Type = {
    val next_state = Wire(JtagState())
    next_state := cur

    switch(cur) {
      is(TestLogicReset) {
        when(tms_i) {
          next_state := TestLogicReset
        }.otherwise {
          next_state := RunTestIdle
        }
      }
      is(RunTestIdle) {
        when(tms_i) {
          next_state := SelectDrScan
        }.otherwise {
          next_state := RunTestIdle
        }
      }
      is(SelectDrScan) {
        when(tms_i) {
          next_state := SelectIrScan
        }.otherwise {
          next_state := CaptureDr
        }
      }
      is(CaptureDr) {
        when(tms_i) {
          next_state := Exit1Dr
        }.otherwise {
          next_state := ShiftDr
        }
      }
      is(ShiftDr) {
        when(tms_i) {
          next_state := Exit1Dr
        }.otherwise {
          next_state := ShiftDr
        }
      }
      is(Exit1Dr) {
        when(tms_i) {
          next_state := UpdateDr
        }.otherwise {
          next_state := PauseDr
        }
      }
      is(PauseDr) {
        when(tms_i) {
          next_state := Exit2Dr
        }.otherwise {
          next_state := PauseDr
        }
      }
      is(Exit2Dr) {
        when(tms_i) {
          next_state := UpdateDr
        }.otherwise {
          next_state := ShiftDr
        }
      }
      is(UpdateDr) {
        when(tms_i) {
          next_state := SelectDrScan
        }.otherwise {
          next_state := RunTestIdle
        }
      }
      is(SelectIrScan) {
        when(tms_i) {
          next_state := TestLogicReset
        }.otherwise {
          next_state := CaptureIr
        }
      }
      is(CaptureIr) {
        when(tms_i) {
          next_state := Exit1Ir
        }.otherwise {
          next_state := ShiftIr
        }
      }
      is(ShiftIr) {
        when(tms_i) {
          next_state := Exit1Ir
        }.otherwise {
          next_state := ShiftIr
        }
      }
      is(Exit1Ir) {
        when(tms_i) {
          next_state := UpdateIr
        }.otherwise {
          next_state := PauseIr
        }
      }
      is(PauseIr) {
        when(tms_i) {
          next_state := Exit2Ir
        }.otherwise {
          next_state := PauseIr
        }
      }
      is(Exit2Ir) {
        when(tms_i) {
          next_state := UpdateIr
        }.otherwise {
          next_state := ShiftIr
        }
      }
      is(UpdateIr) {
        when(tms_i) {
          next_state := SelectDrScan
        }.otherwise {
          next_state := RunTestIdle
        }
      }
    }
    next_state
  }
}

class JtagDTM(dm_config: DebugModuleConfig) extends Module {
  val io = IO(new Bundle {
    val jtag = new JtagIO(as_master = false)
    val dmi_req = DecoupledIO(new DMIReq)
    val dmi_resp = Flipped(DecoupledIO(new DMIResp(dm_config.abits)))

    val jtag_state = Output(JtagState())
  })

  io.jtag.clear()
  io.dmi_req.noenq()
  io.dmi_resp.nodeq()

  val jtag_state = RegInit(JtagState.TestLogicReset)
  jtag_state := JtagState.get_next_state(jtag_state, io.jtag.tms)

  io.jtag_state := jtag_state
}

object GenJtagDTM_verilog extends App {
  val dm_config = new DebugModuleConfig()
  GenVerilogHelper(
    new JtagDTM(dm_config)
  )
}
