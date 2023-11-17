package leesum.ICache

import chisel3._
import chisel3.util.{Decoupled, Enum, is, switch}
import chiseltest.ChiselScalatestTester
import chiseltest.formal.{
  BoundedCheck,
  CVC4EngineAnnotation,
  Formal,
  past,
  stable
}
import leesum.axi4.{AXIDef, AXIMasterIO, StreamFork2}
import leesum._
import org.scalatest.flatspec.AnyFlatSpec

class DCacheReq extends Bundle {
  val paddr = UInt(64.W)
  val size = UInt(2.W)
  val wdata = UInt(64.W)
  val wstrb = UInt(8.W)
  val is_store = Bool()
  val is_mmio = Bool()
  val id = UInt(8.W) // TODO: not implemented now

  def is_load = !is_store
}
class DCacheResp extends Bundle {
  val rdata = UInt(64.W)
  val id = UInt(8.W) // TODO: not implemented now
  val exception = new ExceptionEntry()
}
class DCacheTop(formal: Boolean = false) extends Module {

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new DCacheReq))
    val resp = Decoupled(new DCacheResp)
    val flush = Input(Bool())

    // memory port
    val mem_master = new AXIMasterIO(64, 64)
  })

  val icache_hit = WireInit(false.B)
  val icache_data = RegInit(0.U(64.W))
  val icache_tag = RegInit(0.U(64.W))

  io.mem_master.clear()
  io.mem_master.ar.bits.id := 0.U
  io.mem_master.ar.bits.lock := false.B
  io.mem_master.ar.bits.cache := 0.U
  io.mem_master.ar.bits.prot := 0.U
  io.mem_master.ar.bits.qos := 0.U
  io.mem_master.ar.bits.region := 0.U
  io.mem_master.ar.bits.user := 0.U

  io.mem_master.aw.bits.id := 0.U
  io.mem_master.aw.bits.lock := false.B
  io.mem_master.aw.bits.cache := 0.U
  io.mem_master.aw.bits.prot := 0.U
  io.mem_master.aw.bits.qos := 0.U
  io.mem_master.aw.bits.region := 0.U
  io.mem_master.aw.bits.user := 0.U

  io.req.nodeq()
  io.resp.noenq()

  val sIdle :: sWaitTLBResp :: sSendICacheResp :: sSendRefillReq :: sWaitRefillResp :: sFlushRefillReq :: sFlushRefillResp :: Nil =
    Enum(7)

  val dcache_hit = WireInit(false.B)
  val dcache_data = RegInit(0.U(64.W))
  val dcache_tag = RegInit(0.U(64.W))

  val state = RegInit(sIdle)
  val Dcache_req_buf = RegInit(0.U.asTypeOf(new DCacheReq))

  // --------------------------
  // formal
  // --------------------------
  when(io.flush) {
    assume(io.resp.ready === false.B)

    assert(!io.resp.fire)
    assert(!io.req.fire)
  }

  when(io.mem_master.r.fire) {
    assume(io.mem_master.r.bits.id === 0.U)
    assume(io.mem_master.r.bits.resp === AXIDef.RESP_OKAY)
    assume(io.mem_master.r.bits.last === true.B)
    assume(io.mem_master.r.bits.user === 0.U)
  }

  when(io.mem_master.r.fire) {
    assume(io.mem_master.r.bits.last === true.B)
  }

  if (formal) {
    val f_flush = io.flush | past(io.flush)

    when(FormalUtils.StreamShouldStable(io.req) && !f_flush) {
      assume(io.req.valid)
      assume(stable(io.req.bits))
    }

    when(FormalUtils.StreamShouldStable(io.resp) && !f_flush) {
      assert(io.resp.valid)
      assert(stable(io.resp.bits))
    }

    when(FormalUtils.StreamShouldStable(io.mem_master.ar)) {
      assert(io.mem_master.ar.valid)
      assert(stable(io.mem_master.ar.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.aw)) {
      assert(io.mem_master.aw.valid)
      assert(stable(io.mem_master.aw.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.w)) {
      assert(io.mem_master.w.valid)
      assert(stable(io.mem_master.w.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.r)) {
      assume(io.mem_master.r.valid)
      assume(stable(io.mem_master.r.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.b)) {
      assume(io.mem_master.b.valid)
      assume(stable(io.mem_master.b.bits))
    }
  }
}

class DCacheFormal extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "DCache" should "pass with assumption" in {
    verify(
      new DCacheTop(formal = true),
      Seq(BoundedCheck(20), CVC4EngineAnnotation)
    )
  }
}

object gen_dcache_top_verilog extends App {
  GenVerilogHelper(new DCacheTop)
}
