package leesum.Utils

import chisel3.util._
import chisel3.{Module, _}
import leesum.GenVerilogHelper

import scala.collection.Seq

class SinglePortSRAM[T <: Data](nums: Int, tpe: T) extends Module {
  require(isPow2(nums) && nums > 1, "nums should be power of 2")
  val addr_width = log2Ceil(nums)
  val io = IO(new Bundle {
    val addr = Input(UInt(addr_width.W))
    val rdata = Output(tpe)
    val en = Input(Bool())
    val wen = Input(Bool())
    val wdata = Input(tpe)
  })

  val sram_one_port = SRAM(nums, tpe, 0, 0, 1)

  sram_one_port.readwritePorts(0).address := io.addr
  sram_one_port.readwritePorts(0).writeData := io.wdata
  sram_one_port.readwritePorts(0).isWrite := io.wen
  sram_one_port.readwritePorts(0).enable := io.en

  // keep the last read data when current operation is read
  val read_op = io.en && !io.wen
  io.rdata := HoldRegister(read_op, sram_one_port.readwritePorts(0).readData, 1)
}

class DualPortSRAM[T <: Data](nums: Int, tpe: T) extends Module {
  require(isPow2(nums) && nums > 1, "nums should be power of 2")
  val addr_width = log2Ceil(nums)
  val io = IO(new Bundle {
    // read
    val raddr = Input(UInt(addr_width.W))
    val ren = Input(Bool())
    val rdata = Output(tpe)
    // write
    val waddr = Input(UInt(addr_width.W))
    val wen = Input(Bool())
    val wdata = Input(tpe)
  })

  val sram_dual_port = SRAM(nums, tpe, 1, 1, 0)

  sram_dual_port.readPorts(0).address := io.raddr
  sram_dual_port.readPorts(0).enable := io.ren

  sram_dual_port.writePorts(0).address := io.waddr
  sram_dual_port.writePorts(0).data := io.wdata
  sram_dual_port.writePorts(0).enable := io.wen

  // keep the last read data when current operation is read
  val read_op = io.ren
  io.rdata := HoldRegister(
    read_op,
    sram_dual_port.readPorts(0).data,
    1
  )
}

class NegedgeDataModuleTemplate[T <: Data](
    gen: T,
    numEntries: Int,
    numRead: Int,
    numWrite: Int,
    parentModule: String,
    perReadPortBypassEnable: Option[Seq[Boolean]] = None
) extends Module {
  val io = IO(new Bundle {
    val raddr = Vec(numRead, Input(UInt(log2Ceil(numEntries).W)))
    val rdata = Vec(numRead, Output(gen))
    val wen = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(UInt(log2Ceil(numEntries).W)))
    val wdata = Vec(numWrite, Input(gen))
  })

  override def desiredName: String =
    s"NegedgeDataModule_${parentModule}_${numEntries}entry"
  val data = Reg(Vec(numEntries, gen))

  // if use bypassEnable to control bypass of each port,
  // then we should have a separate bit for each read port
  perReadPortBypassEnable.map(en_vec => require(en_vec.length == numRead))
  // read ports
  for (i <- 0 until numRead) {
    val bypass_en = perReadPortBypassEnable.map(_(i)).getOrElse(true)
    val read_by =
      io.wen.zip(io.waddr).map(w => w._1 && w._2 === io.raddr(i) && bypass_en.B)
    val addr_dec = UIntToOH(io.raddr(i), numEntries)
    when(VecInit(read_by).asUInt.orR) {
      io.rdata(i) := Mux1H(read_by, io.wdata)
    }.otherwise {
      io.rdata(i) := Mux1H(addr_dec, data)
    }
  }

  // write ports
  val waddr_dec = io.waddr.map(a => UIntToOH(a))
  for (j <- 0 until numEntries) {
    val write_wen = io.wen.zip(waddr_dec).map(w => w._1 && w._2(j))
    when(VecInit(write_wen).asUInt.orR) {
      data(j) := Mux1H(write_wen, io.wdata)
    }
  }
}

class SyncDataModuleTemplate[T <: Data](
    gen: T,
    numEntries: Int,
    numRead: Int,
    numWrite: Int,
    parentModule: String = "",
    concatData: Boolean = false,
    perReadPortBypassEnable: Option[Seq[Boolean]] = None
) extends Module {
  val io = IO(new Bundle {
    val raddr = Vec(numRead, Input(UInt(log2Ceil(numEntries).W)))
    val rdata = Vec(numRead, Output(gen))
    val wen = Vec(numWrite, Input(Bool()))
    val waddr = Vec(numWrite, Input(UInt(log2Ceil(numEntries).W)))
    val wdata = Vec(numWrite, Input(gen))
  })

  override def desiredName: String =
    s"SyncDataModuleTemplate_${parentModule}_${numEntries}entry"
  val dataType = if (concatData) UInt(gen.getWidth.W) else gen

  val maxBankEntries = if (numEntries >= 2 * 64) 64 else 16
  val numBanks = (numEntries + maxBankEntries - 1) / maxBankEntries
  def bankOffset(address: UInt): UInt = {
    if (numBanks > 1) address(log2Ceil(maxBankEntries) - 1, 0)
    else address
  }
  def bankIndex(address: UInt): UInt = {
    if (numBanks > 1)
      address(log2Ceil(numEntries) - 1, log2Ceil(maxBankEntries))
    else 0.U
  }

  // if use bypassEnable to control bypass of each port,
  // then we should have a separate bit for each read port
  perReadPortBypassEnable.map(en_vec => require(en_vec.length == numRead))

  val dataBanks = Seq.tabulate(numBanks)(i => {
    val bankEntries =
      if (i < numBanks - 1) maxBankEntries
      else numEntries - (i * maxBankEntries)
    val dataBank = Module(
      new NegedgeDataModuleTemplate(
        dataType,
        bankEntries,
        numRead,
        numWrite,
        parentModule,
        perReadPortBypassEnable
      )
    )

    // delay one clock
    val raddr_dup = RegNext(io.raddr)
    val wen_dup = RegNext(io.wen)
    val waddr_dup = io.wen.zip(io.waddr).map(w => RegEnable(w._2, w._1))

    // input
    dataBank.io.raddr := raddr_dup.map(bankOffset)
    dataBank.io.wen := wen_dup.zip(waddr_dup).map { case (en, addr) =>
      en && bankIndex(addr) === i.U
    }
    dataBank.io.waddr := waddr_dup.map(bankOffset)
    if (concatData) {
      val wdata_dup =
        io.wen.zip(io.wdata).map(w => RegEnable(w._2.asTypeOf(dataType), w._1))
      dataBank.io.wdata := wdata_dup
    } else {
      dataBank.io.wdata := io.wen.zip(io.wdata).map(w => RegEnable(w._2, w._1))
    }

    dataBank
  })

  // output
  val rdata =
    if (concatData) dataBanks.map(_.io.rdata.map(_.asTypeOf(gen)))
    else dataBanks.map(_.io.rdata)
  for (j <- 0 until numRead) {
    val raddr_dup = RegNext(io.raddr(j))
    val index_dec = UIntToOH(bankIndex(raddr_dup), numBanks)

    val x = rdata.map(_(j)).toSeq
    val y = index_dec.asBools

    io.rdata(j) := Mux1H(y, x)
  }
}

object gen_NegedgeDataModuleTemplate_verilog extends App {
  GenVerilogHelper(
    new SyncDataModuleTemplate(
      UInt(8.W),
      1024,
      4,
      4,
      "test"
    )
  )
}

object gen_SinglePortSRAM_verilog extends App {
  GenVerilogHelper(new SinglePortSRAM(64, UInt(64.W)))
}

object gen_DualPortSRAM_verilog extends App {
  GenVerilogHelper(new DualPortSRAM(64, UInt(64.W)))
}
