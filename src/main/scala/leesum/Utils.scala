package leesum

import _root_.circt.stage.ChiselStage
import chisel3._
import chisel3.util._
import leesum.Utils.DecoderHelper

import java.io.{File, FileOutputStream, PrintWriter}
import scala.sys.process._

object GenVerilogHelper {
  def rm_anno_file(): Unit = {
    val command = Seq("sh", "-c", "rm *.anno.json")
    val result = command.!! // execute the command
    println(result)
  }
  def apply(gen: => RawModule, path: String = ""): Unit = {
    val projectDir = System.getProperty("user.dir")

    val verilogDir = s"$projectDir/gen_verilog"
    val file_path = if (path.trim().nonEmpty) {
      path
    } else {
      verilogDir + '/' + "test.sv"
    }

    val x = ChiselStage.emitSystemVerilog(
      gen = gen,
      args = Array(
        "--firtool-binary-path",
        "firtool"
//        "--warnings-as-errors"
//        "--throw-on-first-error"
      ),
      firtoolOpts = Array(
        "--disable-all-randomization",
        "--strip-debug-info",
        "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb,verifLabels",
//        "--split-verilog",
//        "--lowering-options=disallowLocalVariables",
        "--lower-memories",
        "--ignore-read-enable-mem",
        "-o=" + file_path,
        "-O=release"
      )
    )
    // rm_anno_file()
  }
}

// TODO: add comments
object CheckUnique {
  def apply[T <: Data](vec: Vec[T], exclude: T): Bool = {
    val conflict = vec
      .combinations(2)
      .map { case Seq(a, b) =>
        (a === b) && !(a === exclude)
      }
      .reduce(_ || _)

    !conflict
  }
}

object CheckOverlap {
  def apply(addr: UInt, range_start: UInt, range_end: UInt): Bool = {
    assert(range_start < range_end, "range_start must be less than range_end")
    val overlap = (addr >= range_start) && (addr < range_end)
    overlap
  }
  def apply(addr: UInt, range: Seq[(UInt, UInt)]): Bool = {
    val overlap = range
      .map({ case (start, end) =>
        apply(addr, start, end)
      })
      .reduce(_ || _)
    overlap
  }
}

object InRange {
  def apply(addr: UInt, range_start: UInt, range_end: UInt): Bool = {
    assert(range_start < range_end, "range_start must be less than range_end")
    val in_range = (addr >= range_start) && (addr < range_end)
    in_range
  }
  def apply(addr: UInt, range: Seq[(UInt, UInt)]): Bool = {
    val in_range = range
      .map({ case (start, end) =>
        apply(addr, start, end)
      })
      .reduce(_ || _)
    in_range
  }
}

/** only if vec(i-1) is valid, vec(i) can be valid. 0000 -> pass check 0001 ->
  * pass check 0011 -> pass check 0111 -> pass check 1111 -> pass check 1000 ->
  * fail check 1001 -> fail check 1100 -> fail check
  */
object CheckOrder {
  def apply(vec: Iterable[Bool]): Bool = {
    checkOrder_internal(vec, vec.size - 1)
  }

  private def checkOrder_internal(vec: Iterable[Bool], idx: Int): Bool = {
    val vec_seq = vec.toSeq
    idx match {
      case 0 => true.B
      case _ =>
        (vec_seq(idx - 1) || !vec_seq(idx)) && checkOrder_internal(
          vec,
          idx - 1
        )
    }
  }
}

/** Generate a vector of Bool, where the i-th element is true if and only if all
  * the elements before it are true
  */
object GenOrderVec {
  def apply(inputVector: Vec[Bool]): Vec[Bool] = {

    val _x = 1
      .to(inputVector.length)
      .map(i => {
        val mask = inputVector.take(i).reduce(_ && _)
        mask
      })

    require(_x.size == inputVector.size)

    VecInit(_x)
  }
}

object gen_GenOrderVec_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val in = Input(Vec(4, Bool()))
      val out = Output(Vec(4, Bool()))
    })
    io.out := GenOrderVec(io.in)
  })
}

/** SignExt is an object that provides methods for sign extending a given UInt.
  * The width of the UInt is specified by input_width. The width of the sign
  * extended UInt is specified by output_width.
  */
object SignExt {
  private def processInput(
      x: UInt,
      input_width: Int,
      output_width: Int
  ): UInt = {
    val sign = x(input_width - 1)
    val sign_ext = Fill(output_width - input_width, sign)
    Cat(sign_ext, x(input_width - 1, 0))
  }

  def apply(x: UInt, input_width: Int, output_width: Int): UInt = {
    require(input_width <= output_width)
    require(x.getWidth >= input_width && x.getWidth <= output_width)

    if (input_width == output_width) x
    else processInput(x, input_width, output_width)
  }

  def apply(x: UInt, input_width: Int, output_width: Int, en: Bool): UInt = {
    require(input_width <= output_width)
    require(x.getWidth >= input_width && x.getWidth <= output_width)

    if (input_width == output_width) x
    else
      Mux(
        en,
        processInput(x, input_width, output_width),
        Cat(0.U((output_width - input_width).W), x(input_width - 1, 0))
      )
  }
}

object ZeroExt {
  def apply(x: UInt, input_width: Int, output_width: Int): UInt = {
    SignExt(x, input_width, output_width, false.B)
  }
}

object GenMaskZero {
  def apply(width: Int, zero_count: Int, start_left: Boolean = false): UInt = {
    require(zero_count <= width)
    if (zero_count == 0) {
      Fill(width, 1.U(1.W))
    } else if (zero_count == width) {
      Fill(width, 0.U(1.W))
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

  def apply(width: Int, zero_count: UInt, start_left: Boolean): UInt = {
    assert(
      zero_count <= width.U,
      "zero_count must be less than width"
    )

    val mask_result = Mux1H(
      0.to(width)
        .map(i => {
          val mask = GenMaskZero(width, i, start_left)
          val eq = zero_count === i.U
          eq -> mask
        })
    )
    require(mask_result.getWidth == width)
    mask_result
  }
}
object GenMaskOne {
  def apply(width: Int, one_count: Int, start_left: Boolean = false): UInt = {
    require(one_count <= width)

    val result = if (one_count == 0) {
      Fill(width, 0.U(1.W))
    } else if (one_count == width) {
      Fill(width, 1.U(1.W))
    } else {
      val mask_one = Fill(one_count, true.B)
      val mask_zero = Fill(width - one_count, false.B)
      if (start_left) {
        Cat(mask_one, mask_zero)
      } else {
        Cat(mask_zero, mask_one)
      }
    }
    require(result.getWidth == width)
    result
  }

  def apply(width: Int, one_count: UInt, start_left: Boolean): UInt = {
    assert(
      one_count <= width.U,
      "one_count must be less than width"
    )

    val mask_result = Mux1H(
      0.to(width)
        .map(i => {
          val mask = GenMaskOne(width, i, start_left)
          val eq = one_count === i.U
          eq -> mask
        })
    )
    require(mask_result.getWidth == width)
    mask_result
  }

}

object gen_maskone_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val one_count = Input(UInt(4.W))
      val mask = Output(UInt(4.W))
    })
    io.mask := GenMaskZero(4, io.one_count, true)
  })
}

/** PopCountOrder(vec) returns the number of valid bits before the first invalid
  */
object PopCountOrder {
  def apply(vec: Iterable[Bool]): UInt = {
    val order_count_width = log2Ceil(vec.size + 1)

    val x = VecInit(vec.toSeq).asUInt

    assert(CheckOrder(vec), "PopCountOrder: vec must be in order")
    val order_count = MuxLookup(
      x,
      0.U
    )(
      0
        .to(vec.size)
        .map(i => {
          val mask = GenMaskOne(vec.size, i)
          mask -> i.U(order_count_width.W)
        })
    )
    require(order_count.getWidth == order_count_width)
    order_count
  }
}

/** from rocket-chip Util
  */
object BarrelShifter {
  private trait ShiftType

  private object LeftShift extends ShiftType

  private object RightShift extends ShiftType

  private object LeftRotate extends ShiftType

  private object RightRotate extends ShiftType

  private def apply[T <: Data](
      inputs: Vec[T],
      shiftInput: UInt,
      shiftType: ShiftType,
      shiftGranularity: Int = 1
  ): Vec[T] = {
    require(shiftGranularity > 0)
    val elementType: T = chiselTypeOf(inputs.head)
    shiftInput.asBools
      .grouped(shiftGranularity)
      .map(VecInit(_).asUInt)
      .zipWithIndex
      .foldLeft(inputs) { case (prev, (shiftBits, layer)) =>
        Mux1H(
          UIntToOH(shiftBits),
          Seq.tabulate(1 << shiftBits.getWidth)(i => {
            // For each layer of barrel shifter, it needs to
            // Mux between shift 0 and i * 2^(depthOfLayer*granularity)
            //
            // e.g, when depth = 2 and granu = 1, the first layer Mux between 0 and 1
            // while the second layer Mux between 0 and 2, thus Vec.shift(UInt(2.W))
            //
            // e.g, when depth = 2 and granu = 2, the first layer Mux between 0, 1, 2 and 3
            // while the second layer Mux between 0, 4, 8, and 12, thus achieving Vec.shift(UInt(4.W))
            //
            // Also, shift no more than inputs length since prev.drop will not warn about overflow
            // this is for Vec with length not the exponential of 2, e.g. 13
            val layerShift: Int =
              (i * (1 << (layer * shiftGranularity))).min(prev.length)
            VecInit(shiftType match {
              case LeftRotate =>
                prev.drop(layerShift) ++ prev.take(layerShift)
              case LeftShift =>
                prev.drop(layerShift) ++ Seq
                  .fill(layerShift)(0.U.asTypeOf(elementType))
              case RightRotate =>
                prev.takeRight(layerShift) ++ prev.dropRight(layerShift)
              case RightShift =>
                Seq.fill(layerShift)(0.U.asTypeOf(elementType)) ++ prev
                  .dropRight(layerShift)
            })
          })
        )
      }
  }

  def leftShift[T <: Data](
      inputs: Vec[T],
      shift: UInt,
      layerSize: Int = 1
  ): Vec[T] =
    apply(inputs, shift, LeftShift, layerSize)

  def rightShift[T <: Data](
      inputs: Vec[T],
      shift: UInt,
      layerSize: Int = 1
  ): Vec[T] =
    apply(inputs, shift, RightShift, layerSize)

  def leftRotate[T <: Data](
      inputs: Vec[T],
      shift: UInt,
      layerSize: Int = 1
  ): Vec[T] =
    apply(inputs, shift, LeftRotate, layerSize)

  def rightRotate[T <: Data](
      inputs: Vec[T],
      shift: UInt,
      layerSize: Int = 1
  ): Vec[T] =
    apply(inputs, shift, RightRotate, layerSize)
}

object gen_shift_right_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val in = Input(Vec(32, UInt(32.W)))
      val shift = Input(UInt(6.W))
      val out = Output(Vec(32, UInt(32.W)))
    })
    io.out := BarrelShifter.rightRotate(io.in, io.shift)
  })
}

object gen_PopCountOrder_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val in = Input(Vec(5, Bool()))
      val out = Output(UInt(3.W))
    })
    io.out := PopCountOrder(io.in)
  })
}

object CheckAligned {
  // Determines if address is aligned to specified size.
  // Size: 0 -> 1 byte, 1 -> 2 bytes, 2 -> 4 bytes, 3 -> 8 bytes
  // Address (addr) should be at least be 3 bits wide and size should be 2 bits.
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

object GenAxiWdata {

  def apply(wdata: UInt, addr: UInt) = {
    apply2(wdata, addr)
  }

  def apply1(wdata: UInt, addr: UInt) = {
    require(
      addr.getWidth >= 3 && wdata.getWidth == 64,
      "GenWdata: addr must be at least 3 bits wide, wdata must be 64 bits wide"
    )
    val offset = addr(2, 0)
    val wdata_vec = wdata.asTypeOf(Vec(8, UInt(8.W)))
    val wdata_aligned = BarrelShifter.rightShift(wdata_vec, offset).asUInt
    wdata_aligned
  }

  def apply2(wdata: UInt, addr: UInt) = {
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
        wdata_aligned := Cat(wdata(63 - 8, 0), 0.U(8.W))
      }
      is(2.U) {
        wdata_aligned := Cat(wdata(63 - 16, 0), 0.U(16.W))
      }
      is(3.U) {
        wdata_aligned := Cat(wdata(63 - 24, 0), 0.U(24.W))
      }
      is(4.U) {
        wdata_aligned := Cat(wdata(63 - 32, 0), 0.U(32.W))
      }
      is(5.U) {
        wdata_aligned := Cat(wdata(63 - 40, 0), 0.U(40.W))
      }
      is(6.U) {
        wdata_aligned := Cat(wdata(63 - 48, 0), 0.U(48.W))
      }
      is(7.U) {
        wdata_aligned := Cat(wdata(63 - 56, 0), 0.U(56.W))
      }
    }
    wdata_aligned
  }
}

object gen_GenAxiWdata_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val wdata = Input(UInt(64.W))
      val addr = Input(UInt(64.W))
      val wdata_aligned = Output(UInt(64.W))
    })
    io.wdata_aligned := GenAxiWdata(io.wdata, io.addr)
  })
}

object GenAxiWstrb {

  def apply(addr: UInt, size: UInt): UInt = {
    apply1(addr, size)
  }

  def apply1(addr: UInt, size: UInt): UInt = {
    require(
      size.getWidth == 2 && addr.getWidth >= 3,
      "GenWstrb: size must be 2 bits and addr must be at least 3 bits wide"
    )
    val size_mask = DecoderHelper(
      size,
      "b0000_0000".U(8.W),
      List(
        BitPat("b00") -> "b0000_0001".U(8.W),
        BitPat("b01") -> "b0000_0011".U(8.W),
        BitPat("b10") -> "b0000_1111".U(8.W),
        BitPat("b11") -> "b1111_1111".U(8.W)
      )
    )
    val beMask = (size_mask << addr(2, 0))(7, 0).asUInt

    require(beMask.getWidth == 8)
    beMask
  }

  def apply2(addr: UInt, size: UInt): UInt = {

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

object gen_GenAxiWstrb_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val addr = Input(UInt(64.W))
      val size = Input(UInt(2.W))
      val wstrb = Output(UInt(8.W))
    })
    io.wstrb := GenAxiWstrb(io.addr, io.size)
  })
}

/** get rdata from axi r channel, shift it to the right position, and fill the
  * rest with 0
  */
class GetAxiRdata extends Module {
  val io = IO(new Bundle {
    val rdata = Input(UInt(64.W))
    val addr = Input(UInt(64.W))
    val size = Input(UInt(2.W))
    val sign_ext = Input(Bool())
    val rdata_aligned = Output(UInt(64.W))
  })

  val rdata = io.rdata
  val size = io.size
  val sign_ext = io.sign_ext
  val offset = io.addr(2, 0)

  // Define a temporary variable to hold the result
  val rdata_aligned = WireInit(rdata)
  switch(size) {
    is(0.U) {
      switch(offset) {
        is(0.U) {
          rdata_aligned := Cat(Fill(56, sign_ext & rdata(7)), rdata(7, 0))
        }
        is(1.U) {
          rdata_aligned := Cat(Fill(56, sign_ext & rdata(15)), rdata(15, 8))
        }
        is(2.U) {
          rdata_aligned := Cat(Fill(56, sign_ext & rdata(23)), rdata(23, 16))
        }
        is(3.U) {
          rdata_aligned := Cat(Fill(56, sign_ext & rdata(31)), rdata(31, 24))
        }
        is(4.U) {
          rdata_aligned := Cat(Fill(56, sign_ext & rdata(39)), rdata(39, 32))
        }
        is(5.U) {
          rdata_aligned := Cat(Fill(56, sign_ext & rdata(47)), rdata(47, 40))
        }
        is(6.U) {
          rdata_aligned := Cat(Fill(56, sign_ext & rdata(55)), rdata(55, 48))
        }
        is(7.U) {
          rdata_aligned := Cat(Fill(56, sign_ext & rdata(63)), rdata(63, 56))
        }
      }
    }
    is(1.U) {
      switch(offset) {
        is(0.U) {
          rdata_aligned := Cat(Fill(48, sign_ext & rdata(15)), rdata(15, 0))
        }
        is(2.U) {
          rdata_aligned := Cat(Fill(48, sign_ext & rdata(31)), rdata(31, 16))
        }
        is(4.U) {
          rdata_aligned := Cat(Fill(48, sign_ext & rdata(47)), rdata(47, 32))
        }
        is(6.U) {
          rdata_aligned := Cat(Fill(48, sign_ext & rdata(63)), rdata(63, 48))
        }
      }
    }
    is(2.U) {
      switch(offset) {
        is(0.U) {
          rdata_aligned := Cat(Fill(32, sign_ext & rdata(31)), rdata(31, 0))
        }
        is(4.U) {
          rdata_aligned := Cat(Fill(32, sign_ext & rdata(63)), rdata(63, 32))
        }
      }
    }
    is(3.U) {
      rdata_aligned := rdata
    }
  }
  io.rdata_aligned := rdata_aligned
}

object GetAxiRdata {
  def apply(rdata: UInt, addr: UInt, size: UInt, sign_ext: Bool): UInt = {
    require(size.getWidth == 2, "size must be 2 bits")
    require(addr.getWidth == 64, "addr must be 64 bits")

    val get_axi_rdata = Module(new GetAxiRdata)
    get_axi_rdata.io.rdata := rdata
    get_axi_rdata.io.addr := addr
    get_axi_rdata.io.size := size
    get_axi_rdata.io.sign_ext := sign_ext
    get_axi_rdata.io.rdata_aligned
  }
}
object gen_GetAxiRdata_verilog extends App {
  GenVerilogHelper(new GetAxiRdata)
}

object GenSizeByAddr {
  def apply(addr: UInt): UInt = {

    val mapping = List(
      BitPat("b000") -> 3.U(2.W), // 8 bytes
      BitPat("b100") -> 2.U(2.W), // 4 bytes
      BitPat("b?10") -> 1.U(2.W), // 2 bytes
      BitPat("b??1") -> 0.U(2.W) // 1 byte
    )

    val size = DecoderHelper(addr(2, 0), 0.U(2.W), mapping)
    size
  }
}

object gen_GenSizeByAddr_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val addr = Input(UInt(64.W))
      val size = Output(UInt(2.W))
    })
    io.size := GenSizeByAddr(io.addr)
  })
}

object writeByteArrayToFile {
  def apply(filePath: String, data: Array[Byte]): Unit = {
    val file = new File(filePath)

    if (file.exists()) {
      file.delete()
    }

    val fos = new FileOutputStream(file)
    try {
      fos.write(data)
    } finally {
      fos.close()
    }
  }
}

object writeByteArrayToStringsToFile {
  def apply(filePath: String, data: Array[Byte]): Unit = {
    val file = new File(filePath)
    if (file.exists()) {
      file.delete()
    }

    val fos = new FileOutputStream(file)
    val pw = new PrintWriter(fos)

    try {
      data.foreach { byte =>
        val unsignedByte = byte.toInt & 0xff
        pw.println(
          Integer.toHexString(unsignedByte).toUpperCase
        )
      }
    } finally {
      pw.close()
      fos.close()
    }
  }
}
object Long2UInt64 {
  def apply(x: Long): UInt = {
    if (x < 0) {
      // because of chisel doesn't support convert a negative number to UInt
      // so we first convert Long to hex string(with prefix x)
      // then convert it to UInt
      val hex_string: String = "x" + x.toHexString
      hex_string.U(64.W)
    } else {
      x.U(64.W)
    }
  }
}

object Long2UInt32 {
  def apply(x: Long): UInt = {
    Long2UInt64(x)(31, 0)
  }
}

object ToAugmented {
  implicit class SeqToAugmentedSeq[T <: Data](private val x: Seq[T])
      extends AnyVal {
    def apply(idx: UInt): T = {
      if (x.size <= 1) {
        x.head
      } else if (!isPow2(x.size)) {
        // For non-power-of-2 seqs, reflect elements to simplify decoder
        val new_seq = x ++ x.takeRight(x.size & -x.size)
        new_seq(idx)
      } else {
        // Ignore MSBs of idx
        val truncIdx =
          if (idx.isWidthKnown && idx.getWidth <= log2Ceil(x.size)) idx
          else (idx | 0.U(log2Ceil(x.size).W))(log2Ceil(x.size) - 1, 0)
        x.zipWithIndex.tail.foldLeft(x.head) { case (prev, (cur, i)) =>
          Mux(truncIdx === i.U, cur, prev)
        }
      }
    }
  }
}

//class RegMap {
//  type ReadFunc = (UInt, UInt) => Valid[UInt]
//  type WriteFunc = (UInt, UInt, UInt) => Valid[UInt]
//
//  val normal_read = (addr: UInt, reg: UInt) => {
//    val read_result = Wire(Valid(UInt(reg.getWidth.W)))
//    read_result.valid := true.B
//    read_result.bits := reg
//    read_result
//  }
//  val normal_write = (addr: UInt, reg: UInt, wdata: UInt) => {
//    val write_result = Wire(Valid(UInt(reg.getWidth.W)))
//    write_result.valid := true.B
//    write_result.bits := wdata
//    reg := write_result.bits
//    write_result
//  }
//
//  val empty_write = (addr: UInt, reg: UInt, wdata: UInt) => {
//    val write_result = Wire(Valid(UInt(reg.getWidth.W)))
//    write_result.valid := true.B
//    write_result.bits := 0.U
//    write_result
//  }
//
//  val csr_map = collection.mutable
//    .Map[
//      Int, // csr_addr
//      (UInt, ReadFunc, WriteFunc) // reg,read_func,write_func
//    ]()
//
//  def print_map(): Unit = {
//    csr_map
//      .map({ case (addr, (_, _, _)) =>
//        addr
//      })
//      .toSeq
//      .sorted
//      .foreach({ addr =>
//        println(f"0x$addr%08x")
//      })
//  }
//
//  def check_width(): Unit = {
//    val reg_width = csr_map.head._2._1.getWidth
//    csr_map.foreach({ case (_, (reg, _, _)) =>
//      require(
//        reg.getWidth == reg_width,
//        "all regs in csr_map must have the same width"
//      )
//    })
//  }
//
//  def in_range(addr: UInt): Bool = {
//    val all_addr = VecInit(csr_map.keys.toSeq.map(Long2UInt32(_)))
//    all_addr.contains(addr)
//  }
//
//  def add_reg(
//      addr: Int,
//      reg: UInt,
//      read_func: ReadFunc,
//      write_func: WriteFunc
//  ) = {
//    require(!csr_map.contains(addr), s"addr $addr already exists")
//    csr_map.addOne(addr, (reg, read_func, write_func))
//    check_width()
//  }
//
//  /** This function is used to read csr register, if success, return a valid
//    * UInt, otherwise return a invalid UInt
//    * @param raddr
//    *   csr address
//    * @return
//    *   Valid(UInt): bits is the read result
//    */
//  def read(raddr: UInt, enable_1h: Boolean = true): Valid[UInt] = {
//    val raddr_map = csr_map.map({ case (addr, (reg, read_func, _)) =>
//      val addr_u = Long2UInt32(addr)
//      val read_result = read_func(addr_u, reg)
//      (addr_u, read_result)
//    })
//
//    val reg_width = csr_map.head._2._1.getWidth
//
//    val default_read = Wire(Valid(UInt(reg_width.W)))
//    default_read.valid := false.B
//    default_read.bits := 0.U
//
//    val raddr_map_1h = csr_map
//      .map({ case (addr, (reg, read_func, _)) =>
//        val addr_u = Long2UInt32(addr)
//        val read_result = read_func(addr_u, reg)
//        (addr_u === raddr, read_result)
//      })
//      .toSeq
//    // at least one csr read result is valid
//    val oneH_valid = raddr_map_1h.map(_._1).reduce(_ || _)
//    val rdata_1h = Mux(oneH_valid, Mux1H(raddr_map_1h), default_read)
//
//    val rdata = MuxLookup(raddr, default_read)(
//      raddr_map.toSeq
//    )
//
//    if (enable_1h) {
//      rdata_1h
//    } else {
//      rdata
//    }
//  }
//
//  /** This function is used to write csr register, if success, return a valid
//    * UInt, otherwise return a invalid UInt
//    * @param waddr
//    *   csr address
//    * @param wdata
//    *   write data
//    * @return
//    *   Valid(UInt): bits is the write result
//    */
//  def write(waddr: UInt, wdata: UInt): Valid[UInt] = {
//    val write_result = Wire(Valid(UInt(wdata.getWidth.W)))
//    write_result.valid := false.B
//    write_result.bits := 0.U
//    csr_map.foreach({ case (addr, (reg, _, write_func)) =>
//      val addr_u = Long2UInt32(addr)
//      when(waddr === addr_u) {
//        write_result := write_func(addr_u, reg, wdata)
//      }
//    })
//    write_result
//  }
//}

object CatReverse {

  def apply[T <: Bits](a: T, r: T*): UInt = {
    Cat((a :: r.toList).reverse)
  }

}

object gen_CatReverse_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val in0 = Input(UInt(4.W))
      val in1 = Input(UInt(4.W))
      val in2 = Input(UInt(8.W))
      val out1 = Output(UInt(16.W))
      val out2 = Output(UInt(16.W))
    })
    io.out1 := CatReverse(io.in0, io.in1, io.in2)
    io.out2 := Cat(io.in0, io.in1, io.in2)
  })
}

class MSBDivFreq(val N: Int) extends Module {
  val io = IO(new Bundle {
    val clk_div = Output(Bool())
  })
  require(N > 1, "N must be greater than 1")
  val cnt = RegInit(0.U(log2Ceil(N).W))
  when(cnt === (N - 1).U) {
    cnt := 0.U
  }.otherwise {
    cnt := cnt + 1.U
  }
  io.clk_div := cnt(log2Ceil(N) - 1) // msb
}

object EdgeDetect {
  def up(x: Bool): Bool = {
    x && !RegNext(x)
  }
  def down(x: Bool): Bool = {
    !x && RegNext(x)
  }
  def change(x: Bool): Bool = {
    x =/= RegNext(x)
  }
}

trait PrefixSum {
  // out[0] = summands[0]
  // out[1] = summands[0] + summands[1]
  // out[2] = summands[0] + summands[1] + summands[2]
  // ...
  // where + is your associative operator (reflexivity not required)
  // layerOp is called on each level of the circuit
  def apply[T](summands: Seq[T])(
      associativeOp: (T, T) => T,
      layerOp: (Int, Vector[T]) => Vector[T] = idLayer[T] _
  ): Vector[T]
  def layers(size: Int): Int
  def idLayer[T](x: Int, y: Vector[T]) = y
}

// N-1 area, N-1 depth
object RipplePrefixSum extends PrefixSum {
  def layers(size: Int) = if (size == 0) 1 else size
  def apply[T](summands: Seq[T])(
      associativeOp: (T, T) => T,
      layerOp: (Int, Vector[T]) => Vector[T]
  ): Vector[T] = {
    def helper(layer: Int, offset: Int, x: Vector[T]): Vector[T] = {
      if (offset >= x.size) {
        x
      } else {
        helper(
          layer + 1,
          offset + 1,
          layerOp(
            layer,
            Vector.tabulate(x.size) { i =>
              if (i != offset) {
                x(i)
              } else {
                associativeOp(x(i - 1), x(i))
              }
            }
          )
        )
      }
    }
    helper(1, 1, layerOp(0, summands.toVector))
  }
}

// O(NlogN) area, logN depth
object DensePrefixSum extends PrefixSum {
  def layers(size: Int) = if (size == 0) 1 else 1 + log2Ceil(size)
  def apply[T](summands: Seq[T])(
      associativeOp: (T, T) => T,
      layerOp: (Int, Vector[T]) => Vector[T]
  ): Vector[T] = {
    def helper(layer: Int, offset: Int, x: Vector[T]): Vector[T] = {
      if (offset >= x.size) {
        x
      } else {
        helper(
          layer + 1,
          offset << 1,
          layerOp(
            layer,
            Vector.tabulate(x.size) { i =>
              if (i < offset) {
                x(i)
              } else {
                associativeOp(x(i - offset), x(i))
              }
            }
          )
        )
      }
    }
    helper(1, 1, layerOp(0, summands.toVector))
  }
}

object Gather_Rocket {
  // Compress all the valid data to the lowest indices
  def apply[T <: Data](data: Seq[ValidIO[T]]): Vec[T] =
    apply(data, DensePrefixSum)
  def apply[T <: Data](data: Seq[ValidIO[T]], prefixSum: PrefixSum): Vec[T] = {
    val popBits = log2Ceil(data.size)
    val holes = data.map(x => WireInit(UInt(popBits.W), (!x.valid).asUInt))
    apply(data.map(_.bits), prefixSum(holes)(_ + _))
  }
  def apply[T <: Data](
      data: Seq[T],
      holeSum: Seq[UInt],
      layerOp: (Int, Seq[T], Seq[UInt]) => (Seq[T], Seq[UInt]) = idLayer[T] _
  ): Vec[T] = {
    def helper(
        layer: Int,
        offset: Int,
        holeSum0: Vector[UInt],
        data0: Vector[T]
    ): Vector[T] = {
      val (a, b) = layerOp(layer, data0, holeSum0)
      val data = a.toVector
      val holeSum = b.toVector
      if (offset >= data.size) {
        data
      } else {
        val bit = log2Ceil(offset)
        helper(
          layer + 1,
          offset << 1,
          holeSum,
          Vector.tabulate(data.size) { i =>
            if (i + offset >= data.size) {
              data(i)
            } else {
              Mux(holeSum(i + offset - 1)(bit), data(i + offset), data(i))
            }
          }
        )
      }
    }
    VecInit(helper(0, 1, holeSum.toVector, data.toVector))
  }
  def layers(size: Int) = if (size == 0) 1 else 1 + log2Ceil(size)
  def idLayer[T](layer: Int, data: Seq[T], holeSum: Seq[UInt]) = (data, holeSum)
}

object gen_Gather_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val in = Input(Vec(8, ValidIO(UInt(32.W))))
      val out = Output(Vec(8, UInt(32.W)))
    })
//    io.out := Gather(io.in)
    val out_reg = RegInit(VecInit(Seq.fill(8)(0.U(32.W))))

//    out_reg := Gather_new(io.in.map(_.valid), io.in.map(_.bits))
//    out_reg := Gather(io.in)
    out_reg := Gather_LEESUM1(io.in)

    io.out := ShiftRegister(out_reg, 10)

  })
}

object Gather_LEESUM2 {

  def iter_once[T <: Data](
      valid_seq: Seq[Bool],
      data_seq: Seq[T],
      iter_seq: Seq[T],
      cur_layer: Int
  ): Seq[T] = {
    require(
      valid_seq.size == data_seq.size,
      "valid_seq and data_seq must have the same size"
    )
    if (cur_layer == data_seq.size - 1) {
      println(s"cur_layer: $cur_layer")
      println(s"cur_compressed_seq: ${data_seq.last}")
      Seq(data_seq.last)
    } else {
      val next_compressed_seq =
        iter_once(valid_seq, data_seq, iter_seq, cur_layer + 1)

      val cur_valid = valid_seq(cur_layer)
      val cur_data = data_seq(cur_layer)

      println(s"cur_layer: $cur_layer")
      val x = Vector.tabulate(data_seq.size - cur_layer)(i => {
        if (i == 0) {
          Mux(cur_valid, cur_data, next_compressed_seq(i))
        } else if (i == data_seq.size - cur_layer - 1) {
          next_compressed_seq.last
        } else {
          Mux(cur_valid, next_compressed_seq(i - 1), next_compressed_seq(i))
        }

      })

      x
    }
  }

  def apply[T <: Data](
      valid_seq: Seq[Bool],
      data_seq: Seq[T]
  ): Seq[T] = {
    require(
      valid_seq.size == data_seq.size,
      "valid_seq and data_seq must have the same size"
    )
    val compressed_seq = iter_once(valid_seq, data_seq, Seq(), 0)

    require(compressed_seq.size == data_seq.size)
    compressed_seq
  }

}

object Gather_LEESUM1 {
  def apply[T <: Data](data: Seq[ValidIO[T]]): Vec[T] = {
    val popBits = log2Ceil(data.size)
    val holes = data.map(x => WireInit(UInt(popBits.W), (!x.valid).asUInt))
    val prefixHoles = DensePrefixSum(holes)(_ + _)

    val prio_seq = 0.until(data.size).map { i =>
      val pmux_valid = data.drop(i).map(_.valid)
      val pmux_bits = data.drop(i).map(_.bits)
      PriorityMux(pmux_valid, pmux_bits)
    }
    require(prio_seq.size == data.size)

    val compressedData = Wire(Vec(data.size, data.head.bits.cloneType))

    for (i <- 0 until data.size) {
      val tmp_data = VecInit(prio_seq.drop(i))
      val tmp_idx = prefixHoles(i)
      compressedData(i) := tmp_data(tmp_idx)
    }
    compressedData
  }
}

object gather_scala_test extends App {
  def iter_once(
      valid_seq: Seq[Boolean],
      data_seq: Seq[Int],
      iter_seq: Seq[Int],
      cur_layer: Int
  ): Seq[Int] = {
    require(
      valid_seq.size == data_seq.size,
      "valid_seq and data_seq must have the same size"
    )
    if (cur_layer == data_seq.size - 1) {
      println(s"cur_layer: $cur_layer")
      println(s"cur_compressed_seq: ${data_seq.last}")
      Seq(data_seq.last)
    } else {
      val next_compressed_seq =
        iter_once(valid_seq, data_seq, iter_seq, cur_layer + 1)

      val cur_valid = valid_seq(cur_layer)
      val cur_data = data_seq(cur_layer)

      val cur_compressed_valid_seq = Seq(cur_data) ++ next_compressed_seq
      val cur_compressed_novalid_seq = next_compressed_seq ++ Seq(0)
      val cur_compressed_seq =
        if (cur_valid) cur_compressed_valid_seq else cur_compressed_novalid_seq

      println(s"cur_layer: $cur_layer")
      println(s"cur_compressed_seq: $cur_compressed_seq")
      cur_compressed_seq
    }
  }

  val gathered_seq =
    iter_once(Seq(true, false, false, true), Seq(1, 2, 3, 4), Seq(), 0)
  println(gathered_seq)

}

class BitMaskHelper {
  def gen_mask(left: Int, right: Int): Long = {
    require(right >= 0 && right < 64, "right must be in the range [0, 63]")
    require(left >= right && left < 64, "left must be in the range [right, 63]")

    val mask = (1L << (left - right + 1)) - 1
    mask << right
  }

  def gen_mask(pos: Int): Long = {
    gen_mask(pos, pos)
  }

  def all = gen_mask(63, 0)
}
