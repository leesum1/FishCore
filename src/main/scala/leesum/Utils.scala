package leesum

import chisel3._
import chisel3.util.{Cat, Fill}
import circt.stage.ChiselStage
import scala.sys.process._

object GenVerilogHelper {
  def rm_anno_file(): Unit = {
    val command = Seq("sh", "-c", "rm *.anno.json")
    val result = command.!! // execute the command
    println(result)
  }
  def apply(gen: => RawModule): Unit = {
    val projectDir = System.getProperty("user.dir")

    val verilogDir = s"$projectDir/gen_verilog"
    val file_path = verilogDir + '/' + "test.sv"

    val x = ChiselStage.emitSystemVerilog(
      gen = gen,
      firtoolOpts = Array(
        "--disable-all-randomization",
        "--strip-debug-info",
        "--lowering-options=disallowLocalVariables,disallowPackedArrays",
        //        "--split-verilog",
        "--lower-memories",
        "--ignore-read-enable-mem",
        "-o=" + file_path,
        "-O=release"
      )
    )
    rm_anno_file()
  }
}

/** only if vec(i-1) is valid, vec(i) can be valid. 0000 -> pass check 0001 ->
  * pass check 0011 -> pass check 0111 -> pass check 1111 -> pass check 1000 ->
  * fail check 1001 -> fail check 1100 -> fail check
  */
object CheckOrder {
  def apply(vec: Vec[Bool]): Bool = {
    checkOrder_internal(vec, vec.length - 1)
  }

  private def checkOrder_internal(vec: Vec[Bool], idx: Int): Bool = {
    idx match {
      case 0 => true.B
      case _ =>
        ((vec(idx - 1) || !vec(idx))) && checkOrder_internal(vec, idx - 1)
    }
  }
}

object SignExt {
  def apply(x: UInt, input_width: Int, output_width: Int): UInt = {
    val sign = x(input_width - 1)
    val sign_ext = Fill(output_width - input_width, sign)
    Cat(sign_ext, x)
  }
}
object GenMaskZero {
  def apply(width: Int, zero_count: Int, start_left: Boolean = false): UInt = {
    require(zero_count <= width)
    if (zero_count == 0) {
      Fill(width, 1.U(1.W))
    } else {
      val mask_one = Fill(width - zero_count, true.B)
      val mask_zero = Fill(zero_count, false.B)
      if (start_left) {
        Cat(mask_zero, mask_one)
      } else {
        Cat(mask_one, mask_zero)
      }
    }
  }
}
object GenMaskOne {
  def apply(width: Int, one_count: Int, start_left: Boolean = false): UInt = {
    require(one_count <= width)
    if (one_count == 0) {
      Fill(width, 0.U(1.W))
    } else {
      val mask_one = Fill(one_count, true.B)
      val mask_zero = Fill(width - one_count, false.B)
      if (start_left) {
        Cat(mask_one, mask_zero)
      } else {
        Cat(mask_zero, mask_one)
      }
    }
  }
}
