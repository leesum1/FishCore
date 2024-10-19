package leesum.dbg

import chisel3.util.{is, switch}
import chisel3._
import leesum.dbg.JtagState.Value

object JtagState extends ChiselEnum {
  val TestLogicReset = Value(0.U)
  val RunTestIdle = Value(1.U)
  val SelectDrScan = Value(2.U)
  val CaptureDr = Value(3.U)
  val ShiftDr = Value(4.U)
  val Exit1Dr = Value(5.U)
  val PauseDr = Value(6.U)
  val Exit2Dr = Value(7.U)
  val UpdateDr = Value(8.U)
  val SelectIrScan = Value(9.U)
  val CaptureIr = Value(10.U)
  val ShiftIr = Value(11.U)
  val Exit1Ir = Value(12.U)
  val PauseIr = Value(13.U)
  val Exit2Ir = Value(14.U)
  val UpdateIr = Value(15.U)

  def is_update_dr(): Bool = {
    this.Value === UpdateDr
  }

  def is_test_logic_reset(): Bool = {
    this.Value === TestLogicReset
  }

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
