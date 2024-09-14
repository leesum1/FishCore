package leesum.Utils

import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import leesum.GenVerilogHelper

class RegManager {
  type ReadFunc = (UInt, UInt) => Valid[UInt]
  type WriteFunc = (UInt, UInt, UInt) => Valid[UInt]

  var max_reg_width = 0

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
    write_result.bits := DontCare
    write_result
  }

  val empty_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(reg.getWidth.W)))
    read_result.valid := false.B
    read_result.bits := DontCare
    read_result
  }

  // 使用不可变的 Map 来存储 CSR 寄存器映射，确保代码的不可变性
  private var csr_map = Map[Int, (UInt, ReadFunc, WriteFunc)]()

  def add_csr(
      addr: Int,
      reg: UInt,
      read_func: ReadFunc,
      write_func: WriteFunc
  ) = {

    require(addr >= 0 && addr < 4096, s"csr addr $addr should be in [0, 4096)")
    require(!csr_map.contains(addr), s"csr addr $addr already exists")

    max_reg_width = max_reg_width max reg.getWidth
    csr_map += (addr -> (reg, read_func, write_func))
  }

  /** This function is used to read csr register, if success, return a valid
    * UInt, otherwise return a invalid UInt
    * @param raddr
    *   csr address
    * @return
    *   Valid(UInt): bits is the read result
    */
  def read(raddr: UInt, use_one_hot: Boolean = true): Valid[UInt] = {
    // 定义默认读取结果
    val defaultRead = Wire(Valid(UInt(max_reg_width.W)))
    defaultRead.valid := false.B
    defaultRead.bits := DontCare

    // 封装读取逻辑，避免重复代码
    def create_read_result(
        addr: Int,
        reg: UInt,
        read_func: (UInt, UInt) => Valid[UInt]
    ): Valid[UInt] = {
      val readResult = read_func(addr.U, reg)
      val readResultPad = Wire(Valid(UInt(max_reg_width.W)))
      readResultPad.valid := readResult.valid
      readResultPad.bits := readResult.bits.pad(max_reg_width)
      readResultPad
    }

    // 构建 CSR 读取映射
    val raddr_map = csr_map.map { case (addr, (reg, read_func, _)) =>
      addr.U -> create_read_result(addr, reg, read_func)
    }.toSeq

    // 构建 one-hot 读取映射
    val raddr_map1H = csr_map.map { case (addr, (reg, read_func, _)) =>
      (addr.U === raddr) -> create_read_result(addr, reg, read_func)
    }.toSeq

    // 处理 one-hot 的逻辑
    val oneHValid = raddr_map1H.map(_._1).reduce(_ || _)
    val rdata1H = Mux(oneHValid, Mux1H(raddr_map1H), defaultRead)

    // 处理普通多路选择逻辑
    val rdata = MuxLookup(raddr, defaultRead)(raddr_map)

    // 根据参数返回 one-hot 或 MuxLookup 结果
    if (use_one_hot) rdata1H else rdata
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
    val write_result_pad = Wire(Valid(UInt(max_reg_width.W)))
    write_result_pad.valid := false.B
    write_result_pad.bits := DontCare

    for ((addr, (reg, _, write_func)) <- csr_map) {
      prefix(s"waddr_${addr.toHexString}") {
        when(waddr === addr.U) {
          val __write_result =
            write_func(addr.U, reg, wdata(reg.getWidth - 1, 0))
          write_result_pad.valid := __write_result.valid
          write_result_pad.bits := __write_result.bits.pad(max_reg_width)
        }
      }
    }

    write_result_pad
  }
}

object GenCSRMAP2VerilogTest extends App {
  GenVerilogHelper(
    new Module {

      val io = IO(new Bundle {
        val read_en = Input(Bool())
        val read_addr = Input(UInt(12.W))
        val read_data = Output(UInt(64.W))
        val write_en = Input(Bool())
        val write_addr = Input(UInt(12.W))
        val write_data = Input(UInt(64.W))
        val write_ex_resp = Output(Bool())
      })

      io.write_ex_resp := false.B
      io.read_data := 0.U

      val csr_map = new RegManager()
      val reg1 = RegInit(0.U(8.W))
      val reg2 = RegInit(0.U(16.W))
      val reg3 = RegInit(0.U(32.W))
      val reg4 = RegInit(0.U(64.W))

      csr_map.add_csr(0, reg1, csr_map.normal_read, csr_map.normal_write)
      csr_map.add_csr(1, reg2, csr_map.normal_read, csr_map.normal_write)
      csr_map.add_csr(2, reg3, csr_map.normal_read, csr_map.normal_write)
      csr_map.add_csr(3, reg4, csr_map.normal_read, csr_map.normal_write)

      when(io.read_en) {
        io.read_data := csr_map.read(io.read_addr).bits
      }

      when(io.write_en) {
        io.write_ex_resp := csr_map
          .write(io.write_addr, io.write_data)
          .valid === false.B
      }
    }
  )
}
