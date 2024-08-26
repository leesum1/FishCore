package leesum

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import leesum.dbg.DbgPKG._
import leesum.dbg.{DMIReq, DMIResp, DebugModule, DebugModuleConfig}
import org.scalatest.freespec.AnyFreeSpec

class DebugModuleTest extends AnyFreeSpec with ChiselScalatestTester {
  val dm_config = new DebugModuleConfig()

  "DMI hs test" in {
    test(new DebugModule(dm_config))
      .withAnnotations(
        Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
      ) { dut =>
        // -------------------
        // init ports
        // -------------------

        dut.clock.step(10)

        dut.io.dmi_req.enqueue(
          new DMIReq(6)
            .Lit(
              _.addr -> PROGBUF_BASE.U,
              _.data -> 0xffdd.U,
              _.op -> DMI_OP_WRITE.U
            )
        )

        dut.clock.step(5)

        dut.io.dmi_resp.expectDequeue(
          new DMIResp(6)
            .Lit(
              _.addr -> PROGBUF_BASE.U,
              _.data -> 0xffdd.U,
              _.op -> DMI_OP_STATUS_SUCCESS.U
            )
        )

        dut.io.dmi_req.enqueue(
          new DMIReq(6)
            .Lit(
              _.addr -> PROGBUF_BASE.U,
              _.data -> 0.U,
              _.op -> DMI_OP_READ.U
            )
        )

        dut.io.dmi_resp.expectDequeue(
          new DMIResp(6)
            .Lit(
              _.addr -> PROGBUF_BASE.U,
              _.data -> 0xffdd.U,
              _.op -> DMI_OP_STATUS_SUCCESS.U
            )
        )

        dut.io.dmi_req.enqueue(
          new DMIReq(6)
            .Lit(
              _.addr -> 0.U,
              _.data -> 0.U,
              _.op -> DMI_OP_READ.U
            )
        )

        dut.io.dmi_resp.expectDequeue(
          new DMIResp(6)
            .Lit(
              _.addr -> 0.U,
              _.data -> 0.U,
              _.op -> DMI_OP_STATUS_FAILED.U
            )
        )

      }
  }

}
