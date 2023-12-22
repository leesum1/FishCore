package leesum.Cache

import chisel3._
import chisel3.util.{Cat, DecoupledIO, Enum, Mux1H, is, switch}
import leesum.GenVerilogHelper
import leesum.axi4.AXIMasterIO

class CacheWBbundle extends Bundle {
  val cacheline_size = 128
  val cacheline_size_by8byte = 128 / 64

  val tag = UInt(29.W)
  val idx = UInt(6.W)
  val cacheline = UInt(cacheline_size.W)

  def addr = Cat(tag, idx, 0.U(4.W))

  def get_8byte_from_cacheline(offset_by8byte: UInt): UInt = {
    assert(offset_by8byte < cacheline_size_by8byte.U)

    val cacheline_by8byte =
      cacheline.asTypeOf(Vec(cacheline_size_by8byte, UInt(64.W)))
    cacheline_by8byte(offset_by8byte)
  }
}

class CacheWriteBackFSM extends Module {
  val io = IO(new Bundle {
    val wb_in = Flipped(DecoupledIO(new CacheWBbundle))
    val axi_master = new AXIMasterIO(32, 64)
  })

  io.wb_in.nodeq()
  io.axi_master.clear()

  val sIdle :: sWriteback :: sWaitBresp :: Nil = Enum(3)

  val state = RegInit(sIdle)
  val wlen_buf = RegInit(0.U(8.W))
  val is_wlast = wlen_buf === 1.U

  switch(state) {
    is(sIdle) {
      when(io.wb_in.valid) {
        io.axi_master.aw.valid := true.B
        io.axi_master.aw.bits.addr := io.wb_in.bits.addr(31, 0)
        io.axi_master.aw.bits.len := (io.wb_in.bits.cacheline_size_by8byte - 1).U
        io.axi_master.aw.bits.size := DcacheConst.SIZE8
        io.axi_master.aw.bits.burst := DcacheConst.BURST_INCR
        io.axi_master.aw.bits.id := 0.U
        when(io.axi_master.aw.fire) {
          wlen_buf := 0.U
          state := sWriteback
        }
      }
    }
    is(sWriteback) {
      io.axi_master.w.valid := true.B
      io.axi_master.w.bits.data := io.wb_in.bits.get_8byte_from_cacheline(
        wlen_buf
      )
      io.axi_master.w.bits.strb := "b1111_1111".U
      io.axi_master.w.bits.last := is_wlast
      when(io.axi_master.w.fire) {
        wlen_buf := wlen_buf + 1.U
        when(is_wlast) {
          state := sWaitBresp
        }
      }

    }
    is(sWaitBresp) {
      io.axi_master.b.ready := true.B
      when(io.axi_master.b.fire) {
        io.wb_in.ready := true.B
        assert(io.axi_master.b.bits.resp === 0.U)
        assert(io.wb_in.fire)
        state := sIdle
      }
    }
  }

}

object gen_cache_write_back_fsm_verilog extends App {
  GenVerilogHelper(new CacheWriteBackFSM)
}
