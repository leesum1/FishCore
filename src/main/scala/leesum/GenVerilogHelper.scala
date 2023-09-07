package leesum

import chisel3.RawModule
import circt.stage.ChiselStage

object GenVerilogHelper {
  def apply(gen: => RawModule): Unit = {
    val projectDir = System.getProperty("user.dir")

    val verilogDir = s"$projectDir/gen_verilog"

    val file_path = verilogDir + '/' + "test.sv"

    ChiselStage.emitSystemVerilog(
      gen = gen,
      firtoolOpts = Array(
        "--disable-all-randomization",
        "--strip-debug-info",
        "--lowering-options=disallowLocalVariables,disallowPackedArrays",
//        "--split-verilog",
        "-o=" + file_path,
        "-O=release"
      )
    )
  }
}
