package leesum

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

import java.io.{File, FileOutputStream, PrintWriter}
import scala.collection.IterableOnce.iterableOnceExtensionMethods
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
    val maskVector = Wire(Vec(inputVector.length, Bool()))

    // The first element in the mask vector
    maskVector(0) := inputVector(0)

    // Generate the mask for the rest of the elements
    for (i <- 1 until inputVector.length) {
      maskVector(i) := maskVector(i - 1) && inputVector(i)
    }
    maskVector
  }
}

object SignExt {
  def apply(x: UInt, input_width: Int, output_width: Int): UInt = {
    require(input_width <= output_width)
    require(x.getWidth >= input_width && x.getWidth <= output_width)

    if (input_width == output_width) {
      x
    } else {
      val sign = x(input_width - 1)
      val sign_ext = Fill(output_width - input_width, sign)
      Cat(sign_ext, x(input_width - 1, 0))
    }
  }

  def apply(x: UInt, input_width: Int, output_width: Int, en: Bool): UInt = {
    require(input_width <= output_width)
    require(x.getWidth >= input_width && x.getWidth <= output_width)

    if (input_width == output_width) {
      x
    } else {
      Mux(
        en,
        SignExt(x, input_width, output_width),
        Cat(0.U((output_width - input_width).W), x(input_width - 1, 0))
      )
    }
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
}
object GenMaskOne {
  def apply(width: Int, one_count: Int, start_left: Boolean = false): UInt = {
    require(one_count <= width)
    if (one_count == 0) {
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
  }
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

object GenAxiWdata {
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

object GenAxiWstrb {
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
    val size = Lookup(
      addr(2, 0),
      0.U,
      List(
        BitPat("b000") -> 3.U, // 8 bytes
        BitPat("b100") -> 2.U, // 4 bytes
        BitPat("b?10") -> 1.U, // 2 bytes
        BitPat("b??1") -> 0.U // 1 byte
      )
    )
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

class RegMap {
  type ReadFunc = (UInt, UInt) => Valid[UInt]
  type WriteFunc = (UInt, UInt, UInt) => Valid[UInt]

  val normal_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(reg.getWidth.W)))
    read_result.valid := true.B
    read_result.bits := reg
    read_result
  }
  val normal_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(reg.getWidth.W)))
    write_result.valid := true.B
    write_result.bits := wdata
    reg := write_result.bits
    write_result
  }

  val empty_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(reg.getWidth.W)))
    write_result.valid := true.B
    write_result.bits := 0.U
    write_result
  }

  val csr_map = collection.mutable
    .Map[
      Int, // csr_addr
      (UInt, ReadFunc, WriteFunc) // reg,read_func,write_func
    ]()

  def print_map(): Unit = {
    csr_map
      .map({ case (addr, (_, _, _)) =>
        addr
      })
      .toSeq
      .sorted
      .foreach({ addr =>
        println(f"0x$addr%08x")
      })
  }

  def check_width(): Unit = {
    val reg_width = csr_map.head._2._1.getWidth
    csr_map.foreach({ case (_, (reg, _, _)) =>
      require(
        reg.getWidth == reg_width,
        "all regs in csr_map must have the same width"
      )
    })
  }

  def in_range(addr: UInt): Bool = {
    val all_addr = VecInit(csr_map.keys.toSeq.map(Long2UInt32(_)))
    all_addr.contains(addr)
  }

  def add_reg(
      addr: Int,
      reg: UInt,
      read_func: ReadFunc,
      write_func: WriteFunc
  ) = {
    require(!csr_map.contains(addr), s"addr $addr already exists")
    csr_map.addOne(addr, (reg, read_func, write_func))
    check_width()
  }

  /** This function is used to read csr register, if success, return a valid
    * UInt, otherwise return a invalid UInt
    * @param raddr
    *   csr address
    * @return
    *   Valid(UInt): bits is the read result
    */
  def read(raddr: UInt): Valid[UInt] = {
    val raddr_map = csr_map.map({ case (addr, (reg, read_func, _)) =>
      val addr_u = Long2UInt32(addr)
      val read_result = read_func(addr_u, reg)
      (addr_u, read_result)
    })

    val reg_width = csr_map.head._2._1.getWidth

    val default = Wire(Valid(UInt(reg_width.W)))
    default.valid := false.B
    default.bits := 0.U

    // TODO: use reduceTree lookup?
    val rdata = MuxLookup(raddr, default)(
      raddr_map.toSeq
    )
    rdata
  }

  /** This function is used to write csr register, if success, return a valid
    * UInt, otherwise return a invalid UInt
    * @param waddr
    *   csr address
    * @param wdata
    *   write data
    * @return
    *   Valid(UInt): bits is the write result
    */
  def write(waddr: UInt, wdata: UInt): Valid[UInt] = {
    val write_result = Wire(Valid(UInt(wdata.getWidth.W)))
    write_result.valid := false.B
    write_result.bits := 0.U
    csr_map.foreach({ case (addr, (reg, _, write_func)) =>
      val addr_u = Long2UInt32(addr)
      when(waddr === addr_u) {
        write_result := write_func(addr_u, reg, wdata)
      }
    })
    write_result
  }

}
