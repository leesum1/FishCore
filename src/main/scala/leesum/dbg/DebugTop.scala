package leesum.dbg
import chisel3._
import leesum.GenVerilogHelper
import leesum.Utils.CDCHandShakeReqResp

class DebugTop(dm_config: DebugModuleConfig) extends Module {
  val io = IO(new Bundle {
    val jtag_io = new JtagIO(as_master = false)

    //  dm <> core interface
    val debug_core_interface = new DebugModuleCoreInterface
  })

  // ----------------- RISCV Debug Module -----------------
  // Debug Module works in system clock domain
  // 1. Debug Module receives DMI request from JTAG DTM
  //    And sends DMI response to JTAG DTM
  // 2. Debug Module communicates with core, including
  //    R/W GPRs, R/W CSRs, R/W Memory and so on
  // -----------------------------------------------------
  val debug_module = Module(new DebugModule(dm_config))

  //  debug module <> core interface

  io.debug_core_interface <> debug_module.io.debug_core_interface

  // ----------------- JTAG DTM -----------------
  // JTAG DTM works in JTAG clock domain
  // 1. JTAG DTM was controlled by JTAG Raw port
  //    And communicates with Debug Module
  // 2. JTAG DTM will send DMI request to Debug Module
  //    And receive DMI response from Debug Module
  // TODO: when should we reset JTAG DTM?
  // -------------------------------------------
  val jtag_clk = WireInit((io.jtag_io.tck).asClock)
  val jtag_rst = WireInit(io.jtag_io.rst || reset.asBool)
  val jtag_dtm = withClockAndReset(jtag_clk, jtag_rst) {
    Module(new JtagDTM(dm_config))
  }
  // jtag raw port
  jtag_dtm.io.jtag <> io.jtag_io

  // ----------------- CDC Handshake -----------------
  // CDC Handshake between JTAG DTM and Debug Module
  // JTAG DTM <--clkA_domain(jtag clk)--> CDC Handshake <----clkB_domain(system clk)---> DM
  // 1. A(src) domain: JTAG DTM, JTAG clk
  // 2. B(dst) domain: DM, system clk
  // -------------------------------------------------
//  val jtag2dm_cdc_hs = withClockAndReset(
//    jtag_clk,
//    jtag_rst || jtag_dtm.io.jtag_state === JtagState.TestLogicReset
//  ) {
//    Module(
//      new CDCHandShakeReqResp(
//        new DMIReq(dm_config.abits),
//        new DMIResp(dm_config.abits)
//      )
//    )
//  }

  val jtag2dm_cdc_hs = Module(
    new CDCHandShakeReqResp(
      new DMIReq(dm_config.abits),
      new DMIResp(dm_config.abits)
    )
  )

  jtag2dm_cdc_hs.io.clk_src := jtag_clk
  // reset when JTAG TLR or JTAG-TRST
  jtag2dm_cdc_hs.io.rst_src := jtag_rst || jtag_dtm.io.jtag_state === JtagState.TestLogicReset

  jtag2dm_cdc_hs.io.clk_dst := clock
  jtag2dm_cdc_hs.io.rst_dst := reset.asBool

  // A domain: JTAG DTM
  jtag_dtm.io.dmi_req <> jtag2dm_cdc_hs.io.req_src
  jtag_dtm.io.dmi_resp <> jtag2dm_cdc_hs.io.resp_src

  // B domain: DM
  debug_module.io.dmi_req <> jtag2dm_cdc_hs.io.req_dst
  debug_module.io.dmi_resp <> jtag2dm_cdc_hs.io.resp_dst
}

object GenDebugTopVerilog extends App {
  val dm_config = new DebugModuleConfig
  GenVerilogHelper(new DebugTop(dm_config))
}
