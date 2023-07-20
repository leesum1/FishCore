package leesum

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals.AddVecLiteralConstructor
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chiseltest.simulator.WriteVcdAnnotation

class InistFifo_test extends AnyFreeSpec with ChiselScalatestTester {

  "fifo_push_test" in {
    test(new InstsFifo).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      {
        dut.io.in.initSource()
        dut.io.in.setSourceClock(dut.clock)
        dut.io.out.initSink()
        dut.io.out.setSinkClock(dut.clock)

        val input_data = (new InstsItem)
        val input_data_Lit = input_data.Lit(
          _.insts -> Vec(4, UInt(32.W)).Lit(
            0 -> 0x00000013.U,
            1 -> 0x00000014.U,
            2 -> 0x00000015.U,
            3 -> 0x00000016.U
          ),
          _.insts_pc -> Vec(4, UInt(32.W)).Lit(
            0 -> 0x00000000.U,
            1 -> 0x00000004.U,
            2 -> 0x00000008.U,
            3 -> 0x0000000c.U
          ),
          _.insts_valid_mask -> Vec(4, Bool()).Lit(
            0 -> true.B,
            1 -> true.B,
            2 -> true.B,
            3 -> true.B
          ),
          _.insts_rvc_mask -> Vec(4, Bool()).Lit(
            0 -> false.B,
            1 -> false.B,
            2 -> false.B,
            3 -> false.B
          )
        )

        val input_data1 = (new InstsItem)
        val input_data_Lit1 = input_data1.Lit(
          _.insts -> Vec(4, UInt(32.W)).Lit(
            0 -> 0x00000013.U,
            1 -> 0x00000014.U,
            2 -> 0x00000015.U,
            3 -> 0x00000016.U
          ),
          _.insts_pc -> Vec(4, UInt(32.W)).Lit(
            0 -> 0x00000000.U,
            1 -> 0x00000004.U,
            2 -> 0x00000008.U,
            3 -> 0x0000000c.U
          ),
          _.insts_valid_mask -> Vec(4, Bool()).Lit(
            0 -> false.B,
            1 -> true.B,
            2 -> false.B,
            3 -> true.B
          ),
          _.insts_rvc_mask -> Vec(4, Bool()).Lit(
            0 -> false.B,
            1 -> false.B,
            2 -> false.B,
            3 -> false.B
          )
        )

        val input_data2 = (new InstsItem)
        val input_data_Lit2 = input_data2.Lit(
          _.insts -> Vec(4, UInt(32.W)).Lit(
            0 -> 0x00000013.U,
            1 -> 0x00000014.U,
            2 -> 0x00000015.U,
            3 -> 0x00000016.U
          ),
          _.insts_pc -> Vec(4, UInt(32.W)).Lit(
            0 -> 0x00000000.U,
            1 -> 0x00000004.U,
            2 -> 0x00000008.U,
            3 -> 0x0000000c.U
          ),
          _.insts_valid_mask -> Vec(4, Bool()).Lit(
            0 -> false.B,
            1 -> true.B,
            2 -> false.B,
            3 -> false.B
          ),
          _.insts_rvc_mask -> Vec(4, Bool()).Lit(
            0 -> false.B,
            1 -> false.B,
            2 -> false.B,
            3 -> false.B
          )
        )

        val input_data3 = (new InstsItem)
        val input_data_Lit3 = input_data3.Lit(
          _.insts -> Vec(4, UInt(32.W)).Lit(
            0 -> 0x00000013.U,
            1 -> 0x00000014.U,
            2 -> 0x00000015.U,
            3 -> 0x00000016.U
          ),
          _.insts_pc -> Vec(4, UInt(32.W)).Lit(
            0 -> 0x00000000.U,
            1 -> 0x00000004.U,
            2 -> 0x00000008.U,
            3 -> 0x0000000c.U
          ),
          _.insts_valid_mask -> Vec(4, Bool()).Lit(
            0 -> false.B,
            1 -> false.B,
            2 -> false.B,
            3 -> false.B
          ),
          _.insts_rvc_mask -> Vec(4, Bool()).Lit(
            0 -> false.B,
            1 -> false.B,
            2 -> false.B,
            3 -> false.B
          )
        )

        dut.clock.step(5)

        dut.io.in.enqueue(input_data_Lit) // 4
        dut.io.in.enqueue(input_data_Lit1) // 2
        dut.io.in.enqueue(input_data_Lit2) // 1
        dut.io.in.enqueue(input_data_Lit3) // 0
        dut.io.in.enqueue(input_data_Lit2) // 1
        dut.io.in.enqueue(input_data_Lit3) // 0
        dut.clock.step(5)

        dut.io.in.enqueue(input_data_Lit) // 4
        dut.io.in.enqueue(input_data_Lit1) // 2
        dut.io.in.enqueue(input_data_Lit2) // 1
        dut.io.in.enqueue(input_data_Lit3) // 0
        dut.io.in.enqueue(input_data_Lit2) // 1
        dut.io.in.enqueue(input_data_Lit3) // 0

      }
    }

  }

}
