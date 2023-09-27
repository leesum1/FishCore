package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class DummyCacheTest extends AnyFreeSpec with ChiselScalatestTester {

  def gen_load_req(paddr: UInt, size: UInt) = {
    (new LoadDcacheReq).Lit(
      _.paddr -> paddr,
      _.size -> size,
      _.is_mmio -> false.B
    )
  }

  def gen_store_req(
      paddr: UInt,
      wdata: UInt,
      size: UInt,
      wstrb: UInt,
      is_mmio: Boolean = false
  ) = {
    (new StoreDcacheReq).Lit(
      _.paddr -> paddr,
      _.wstrb -> wstrb,
      _.size -> size,
      _.wdata -> wdata,
      _.is_mmio -> is_mmio.B
    )
  }

  "DummyCacheTest1" in {
    test(new DummyDCache)
      .withAnnotations(
        Seq(VerilatorBackendAnnotation)
      ) { dut =>
        dut.io.load_req.initSource().setSourceClock(dut.clock)
        dut.io.load_resp.initSink().setSinkClock(dut.clock)
        dut.io.store_req.initSource().setSourceClock(dut.clock)
        dut.io.store_resp.initSink().setSinkClock(dut.clock)

        val size1 = 0.U
        val size2 = 1.U
        val size4 = 2.U
        val size8 = 3.U

        val store_req1 =
          gen_store_req(0.U, "xdeadbeefdeadbeef".U, size8, "xff".U)
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
          _.data -> "xdeadbeefdeadbeef".U,
          _.exception.valid -> false.B,
          _.exception.tval -> 0.U,
          _.exception.cause -> ExceptionCause.load_access
        )

        val load_req3 = gen_load_req(2.U, size2)
        val load_resp3 = (new LoadDcacheResp).Lit(
          _.data -> "xdeadbeefdeadbeef".U,
          _.exception.valid -> false.B,
          _.exception.tval -> 0.U,
          _.exception.cause -> ExceptionCause.load_access
        )
        val load_req4 = gen_load_req(6.U, size1)
        val load_resp4 = (new LoadDcacheResp).Lit(
          _.data -> "xdeadbeefdeadbeef".U,
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
