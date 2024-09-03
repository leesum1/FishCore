package leesum.dbg
import chisel3._
import chisel3.util.{Decoupled, ValidIO}
import leesum.Cache.{DCacheReq, DCacheResp}
import leesum.{
  CSRReadPort,
  CSRWritePort,
  GPRsWritePort,
  GenVerilogHelper,
  RegFileReadPort
}

class DebugTop(dm_config: DebugModuleConfig) extends Module {
  val io = IO(new Bundle {
    val jtag_io = new JtagIO(as_master = false)

    //  dm <> core interface
    val debug_state_regs = Input(new DbgSlaveState())
    val debug_halt_req = Output(ValidIO(Bool()))
    val debug_resume_req = Output(ValidIO(Bool()))
    val debug_reset_req = Output(ValidIO(Bool()))
    val debug_clear_havereset = Output(ValidIO(Bool()))
    val debug_gpr_read_port = new RegFileReadPort
    val debug_gpr_write_port = Flipped(new GPRsWritePort)
    val debug_csr_read_port = new CSRReadPort
    val debug_csr_write_port = new CSRWritePort
    val debug_dcache_req = Decoupled(new DCacheReq)
    val debug_dcache_resp = Flipped(Decoupled(new DCacheResp))
  })

  val jtag_clk = WireInit((io.jtag_io.tck).asClock)

  val jtag_dtm = withClock(jtag_clk) {
    Module(new JtagDTM(dm_config))
  }

  val debug_module = Module(new DebugModule(dm_config))

  //  dm <> core interface
  debug_module.io.debug_state_regs <> io.debug_state_regs
  debug_module.io.debug_halt_req <> io.debug_halt_req
  debug_module.io.debug_resume_req <> io.debug_resume_req
  debug_module.io.debug_reset_req <> io.debug_reset_req
  debug_module.io.debug_clear_havereset <> io.debug_clear_havereset
  debug_module.io.debug_gpr_read_port <> io.debug_gpr_read_port
  debug_module.io.debug_gpr_write_port <> io.debug_gpr_write_port
  debug_module.io.debug_csr_read_port <> io.debug_csr_read_port
  debug_module.io.debug_csr_write_port <> io.debug_csr_write_port
  debug_module.io.debug_dcache_req <> io.debug_dcache_req
  debug_module.io.debug_dcache_resp <> io.debug_dcache_resp

  //  jtag dmi master <> dm dmi slave
  jtag_dtm.io.dmi_req <> debug_module.io.dmi_req
  jtag_dtm.io.dmi_resp <> debug_module.io.dmi_resp

  // jtagDTM <> jtagIO
  jtag_dtm.io.jtag <> io.jtag_io
}

object GenDebugTopVerilog extends App {
  val dm_config = new DebugModuleConfig
  GenVerilogHelper(new DebugTop(dm_config))
}
