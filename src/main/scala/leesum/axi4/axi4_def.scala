package leesum.axi4

import chisel3._
import chisel3.util.{Decoupled, Mux1H}

object AXIDef {
  def BURST_FIXED = 0.U(2.W)
  def BURST_INCR = 1.U(2.W)
  def BURST_WRAP = 2.U(2.W)
  def BURST_RESERVED = 3.U(2.W)

  def SIZE_1 = 0.U(3.W)
  def SIZE_2 = 1.U(3.W)
  def SIZE_4 = 2.U(3.W)
  def SIZE_8 = 3.U(3.W)
  def SIZE_16 = 4.U(3.W)
  def SIZE_32 = 5.U(3.W)
  def SIZE_64 = 6.U(3.W)
  def SIZE_128 = 7.U(3.W)

  def RESP_OKAY = 0.U(2.W)
  def RESP_EXOKAY = 1.U(2.W)
  def RESP_SLVERR = 2.U(2.W)
  def RESP_DECERR = 3.U(2.W)

  // TODO: size should depend on data width,which could reduce area
  def get_increment_size(size: UInt): UInt = {
    require(size.getWidth == 3)
    val sizeCases = Array(
      (size === SIZE_1) -> 1.U,
      (size === SIZE_2) -> 2.U,
      (size === SIZE_4) -> 4.U,
      (size === SIZE_8) -> 8.U,
      (size === SIZE_16) -> 16.U,
      (size === SIZE_32) -> 32.U,
      (size === SIZE_64) -> 64.U,
      (size === SIZE_128) -> 128.U
    )
    Mux1H(sizeCases)
  }
}
class AXIAddressChannel(val addrWidth: Int) extends Bundle {
  val id = UInt(4.W)
  val addr = UInt(addrWidth.W)
  val len = UInt(8.W)
  val size = UInt(2.W)
  val burst = UInt(2.W)
  // not implemented
  val lock = UInt(1.W)
  val cache = UInt(4.W)
  val prot = UInt(3.W)
  val qos = UInt(4.W)
  val region = UInt(4.W)
  val user = UInt(4.W)
}

class AXIWriteDataChannel(val dataWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
  val strb = UInt((dataWidth / 8).W)
  val last = Bool()
  val user = UInt(4.W)
}

class AXIWriteResponseChannel extends Bundle {
  val id = UInt(4.W)
  val resp = UInt(2.W)
  val user = UInt(4.W)
}

class AXIReadDataChannel(val dataWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
  val id = UInt(4.W)
  val resp = UInt(2.W)
  val last = Bool()
  val user = UInt(4.W)
}

class AXIMasterIO(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val aw = Decoupled(new AXIAddressChannel(addrWidth))
  val w = Decoupled(new AXIWriteDataChannel(dataWidth))
  val b = Flipped(Decoupled(new AXIWriteResponseChannel))
  val ar = Decoupled(new AXIAddressChannel(addrWidth))
  val r = Flipped(Decoupled(new AXIReadDataChannel(dataWidth)))

  def clear(): Unit = {
    aw.valid := false.B
    aw.bits := DontCare
    w.valid := false.B
    w.bits := DontCare
    b.ready := false.B
    ar.valid := false.B
    ar.bits := DontCare
    r.ready := false.B
  }
}

class AXISlaveIO(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val aw = Flipped(Decoupled(new AXIAddressChannel(addrWidth)))
  val w = Flipped(Decoupled(new AXIWriteDataChannel(dataWidth)))
  val b = Decoupled(new AXIWriteResponseChannel)
  val ar = Flipped(Decoupled(new AXIAddressChannel(addrWidth)))
  val r = Decoupled(new AXIReadDataChannel(dataWidth))
  def clear(): Unit = {
    aw.ready := false.B
    w.ready := false.B
    b.valid := false.B
    b.bits := DontCare
    ar.ready := false.B
    r.valid := false.B
    r.bits := DontCare
  }
}
