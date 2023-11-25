package leesum.axi4

import chisel3._
import chisel3.util.{ValidIO, log2Ceil}
import leesum.{GenSizeByAddr, GenVerilogHelper, Long2UInt32, Long2UInt64}
class AddrDecoder(
    addr_map: Seq[(Long, Long, Int)] // (start_addr, end_addr,id)
) extends Module {

  val sel_bits = log2Ceil(addr_map.length + 1)

  val io = IO(new Bundle {
    val addr = Input(ValidIO(UInt(32.W))) // TODO: support 64-bit addr
    val sel_idx = Output(UInt(sel_bits.W))
    val sel_valid = Output(Bool())
    val sel_error = Output(Bool())
  })

  val sel_idx = WireInit(0.U(sel_bits.W))
  val sel_valid = WireInit(false.B)
  val sel_error = WireInit(false.B)

  for (i <- addr_map.indices) {
    val (start_addr, end_addr, id) = addr_map(i)
    when(io.addr.valid) {
      when(
        io.addr.bits >= Long2UInt32(start_addr) && io.addr.bits < Long2UInt32(
          end_addr
        )
      ) {
        sel_idx := id.U
        sel_valid := true.B
      }
    }
  }
  sel_error := io.addr.valid && !sel_valid

  io.sel_idx := sel_idx
  io.sel_valid := sel_valid
  io.sel_error := sel_error
}

object gen_AddrDecoder extends App {
  val addr_map = Seq(
    (0x0L, 0x1000L, 0),
    (0x8000_0000L, 0x8000_2000L, 1),
    (0x2000L, 0x3000L, 2),
    (0x3000L, 0x4000L, 3)
  )

  GenVerilogHelper(
    new AddrDecoder(addr_map)
  )
}
