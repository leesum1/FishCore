package leesum

import chisel3._
import chisel3.util.{Cat, Fill, MuxLookup, is, switch}
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
//        "--lower-memories",
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

object CheckAligned {
  // size: 0 -> 1 byte, 1 -> 2 bytes, 2 -> 4 bytes, 3 -> 8 bytes
  def apply(addr: UInt, size: UInt): Bool = {
    require(
      size.getWidth == 2 && addr.getWidth >= 3,
      "CheckAlign: size must be 2 bits and addr must be at least 3 bits wide"
    )
    val offset = addr(2, 0)
    val aligned = MuxLookup(size, false.B)(
      Seq(
        0.U -> true.B,
        1.U -> (offset(0) === 0.U),
        2.U -> (offset(1, 0) === 0.U),
        3.U -> (offset === 0.U)
      )
    )
    aligned
  }
}

object GenWdataAlign {
  def apply(wdata: UInt, addr: UInt) = {
    require(
      addr.getWidth >= 3 && wdata.getWidth == 64,
      "GenWdata: addr must be at least 3 bits wide, wdata must be 64 bits wide"
    )
    val offset = addr(2, 0)
    val wdata_aligned = WireInit(wdata)

    switch(offset) {
      is(0.U) {
        wdata_aligned := wdata
      }
      is(1.U) {
        wdata_aligned := Cat(wdata(63, 8), 0.U(8.W))
      }
      is(2.U) {
        wdata_aligned := Cat(wdata(63, 16), 0.U(16.W))
      }
      is(3.U) {
        wdata_aligned := Cat(wdata(63, 24), 0.U(24.W))
      }
      is(4.U) {
        wdata_aligned := Cat(wdata(63, 32), 0.U(32.W))
      }
      is(5.U) {
        wdata_aligned := Cat(wdata(63, 40), 0.U(40.W))
      }
      is(6.U) {
        wdata_aligned := Cat(wdata(63, 48), 0.U(48.W))
      }
      is(7.U) {
        wdata_aligned := Cat(wdata(63, 56), 0.U(56.W))
      }
    }
    wdata_aligned
  }
}

object GenWstrb {
  def apply(addr: UInt, size: UInt): UInt = {

    require(
      size.getWidth == 2 && addr.getWidth >= 3,
      "GenWstrb: size must be 2 bits and addr must be at least 3 bits wide"
    )
    val beMask = Wire(UInt(8.W))
    beMask := 0.U
    switch(size) {
      is("b11".U) {
        beMask := "b1111_1111".U
      }
      is("b10".U) {
        switch(addr(2, 0)) {
          is("b000".U) {
            beMask := "b0000_1111".U
          }
          is("b001".U) {
            beMask := "b0001_1110".U
          }
          is("b010".U) {
            beMask := "b0011_1100".U
          }
          is("b011".U) {
            beMask := "b0111_1000".U
          }
          is("b100".U) {
            beMask := "b1111_0000".U
          }
        }
      }
      is("b01".U) {
        switch(addr(2, 0)) {
          is("b000".U) {
            beMask := "b0000_0011".U
          }
          is("b001".U) {
            beMask := "b0000_0110".U
          }
          is("b010".U) {
            beMask := "b0000_1100".U
          }
          is("b011".U) {
            beMask := "b0001_1000".U
          }
          is("b100".U) {
            beMask := "b0011_0000".U
          }
          is("b101".U) {
            beMask := "b0110_0000".U
          }
          is("b110".U) {
            beMask := "b1100_0000".U
          }
        }
      }
      is("b00".U) {
        switch(addr(2, 0)) {
          is("b000".U) {
            beMask := "b0000_0001".U
          }
          is("b001".U) {
            beMask := "b0000_0010".U
          }
          is("b010".U) {
            beMask := "b0000_0100".U
          }
          is("b011".U) {
            beMask := "b0000_1000".U
          }
          is("b100".U) {
            beMask := "b0001_0000".U
          }
          is("b101".U) {
            beMask := "b0010_0000".U
          }
          is("b110".U) {
            beMask := "b0100_0000".U
          }
          is("b111".U) {
            beMask := "b1000_0000".U
          }
        }
      }
    }

    beMask
  }
}

/** get rdata from axi r channel, shift it to the right position, and fill the
  * rest with 0
  */
object GetRdata {
  def apply(rdata: UInt, addr: UInt, size: UInt): UInt = {
    require(size.getWidth == 2, "size must be 2 bits")
    require(addr.getWidth == 64, "addr must be 64 bits")

    val rdata_aligned = Wire(UInt(64.W))
    val offset = addr(2, 0)

    rdata_aligned := rdata
    switch(size) {
      is(0.U) {
        switch(offset) {
          is(0.U) { rdata_aligned := Cat(0.U(56), rdata(7, 0)) }
          is(1.U) { rdata_aligned := Cat(0.U(56), rdata(15, 8)) }
          is(2.U) { rdata_aligned := Cat(0.U(56), rdata(23, 16)) }
          is(3.U) { rdata_aligned := Cat(0.U(56), rdata(31, 24)) }
          is(4.U) { rdata_aligned := Cat(0.U(56), rdata(39, 32)) }
          is(5.U) { rdata_aligned := Cat(0.U(56), rdata(47, 40)) }
          is(6.U) { rdata_aligned := Cat(0.U(56), rdata(55, 48)) }
          is(7.U) { rdata_aligned := Cat(0.U(56), rdata(63, 56)) }
        }
      }
      is(1.U) {
        switch(offset) {
          is(0.U) { rdata_aligned := Cat(0.U(48), rdata(15, 0)) }
          is(2.U) { rdata_aligned := Cat(0.U(48), rdata(31, 16)) }
          is(4.U) { rdata_aligned := Cat(0.U(48), rdata(47, 32)) }
          is(6.U) { rdata_aligned := Cat(0.U(48), rdata(63, 48)) }
        }
      }
      is(2.U) {
        switch(offset) {
          is(0.U) { rdata_aligned := Cat(0.U(32), rdata(31, 0)) }
          is(4.U) { rdata_aligned := Cat(0.U(32), rdata(63, 32)) }
        }
      }
      is(3.U) { rdata_aligned := rdata }
    }

    rdata_aligned
  }
}
