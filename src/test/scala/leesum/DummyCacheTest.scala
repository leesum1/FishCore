package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import leesum.axi4.{StreamFork, skid_buffer}
import leesum.test_utils.gen_rand_uint
import org.scalatest.freespec.AnyFreeSpec
import org.scalacheck.Gen

class DummyCacheTest extends AnyFreeSpec with ChiselScalatestTester {

  def gen_load_req(paddr: UInt, size: UInt) = {
    (new LoadDcacheReq).Lit(
      _.paddr -> paddr,
      _.size -> size,
      _.is_mmio -> false.B
    )
  }
//  class StoreDcacheReq extends Bundle {
//    // must be aligned at 8 bytes
//    val paddr = UInt(64.W)
//    // wdata should be aligned with wstrb
//    val wdata = UInt(64.W)
//    // mask of wdata, same as wstrb in AXI4
//    val wstrb = UInt(8.W)
//    val size = UInt(2.W)
//    val is_mmio = Bool()
//  }
  def gen_store_req(paddr: UInt, wdata: UInt, wstrb: UInt) = {
    (new StoreDcacheReq).Lit(
      _.paddr -> paddr,
      _.wstrb -> wstrb,
      _.size -> 3.U,
      _.wdata -> wdata,
      _.is_mmio -> false.B
    )
  }

  "DummyCacheTest1" in {
    test(new DummyDCache)
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        dut.io.load_req.initSource().setSourceClock(dut.clock)
        dut.io.load_resp.initSink().setSinkClock(dut.clock)
        dut.io.store_req.initSource().setSourceClock(dut.clock)
        dut.io.store_resp.initSink().setSinkClock(dut.clock)

        val size1 = 0.U
        val size2 = 1.U
        val size4 = 2.U
        val size8 = 3.U

        val store_req1 = gen_store_req(0.U, "xdeadbeefdeadbeef".U, "xff".U)
        val store_resp1 = (new StoreDcacheResp).Lit(
          _.exception.valid -> false.B,
          _.exception.tval -> 0.U,
          _.exception.cause -> ExceptionCause.store_access
        )
        val load_req1 = gen_load_req(0.U, size8)
        val load_resp1 = (new LoadDcacheResp).Lit(
          _.data -> "xdeadbeefdeadbeef".U,
          _.exception.valid -> false.B,
          _.exception.tval -> 0.U,
          _.exception.cause -> ExceptionCause.load_access
        )
        val load_req2 = gen_load_req(0.U, size4)
        val load_resp2 = (new LoadDcacheResp).Lit(
          _.data -> "xdeadbeef".U,
          _.exception.valid -> false.B,
          _.exception.tval -> 0.U,
          _.exception.cause -> ExceptionCause.load_access
        )

        val load_req3 = gen_load_req(2.U, size2)
        val load_resp3 = (new LoadDcacheResp).Lit(
          _.data -> "xdead".U,
          _.exception.valid -> false.B,
          _.exception.tval -> 0.U,
          _.exception.cause -> ExceptionCause.load_access
        )
        val load_req4 = gen_load_req(6.U, size1)
        val load_resp4 = (new LoadDcacheResp).Lit(
          _.data -> "xad".U,
          _.exception.valid -> false.B,
          _.exception.tval -> 0.U,
          _.exception.cause -> ExceptionCause.load_access
        )

        dut.clock.step(5)

        dut.io.store_req.enqueue(store_req1)
        dut.io.store_resp.expectDequeue(store_resp1)
        dut.io.load_req.enqueue(load_req1)
        dut.io.load_resp.expectDequeue(load_resp1)
        dut.io.load_req.enqueue(load_req2)
        dut.io.load_resp.expectDequeue(load_resp2)
        dut.clock.step(5)
        dut.io.load_req.enqueue(load_req3)
        dut.io.load_resp.expectDequeue(load_resp3)
        dut.clock.step(5)
        dut.io.load_req.enqueue(load_req4)
        dut.io.load_resp.expectDequeue(load_resp4)

        dut.clock.step(5)

      }
  }

}
