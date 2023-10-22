package leesum
import chisel3._
import chisel3.util.{Decoupled, MuxLookup}

class FuCsrReq extends Bundle {
  val csr_addr = UInt(12.W)
  val rs1_or_zimm = UInt(64.W) // zimm or rs1_data
  val only_read = Bool() // only read, no write, rs1 == x0, or zimm = 0
  val trans_id = UInt(32.W)
  val csr_op = FuOP()
}

class FuCsrResp extends Bundle {
  val data = UInt(64.W)
  val trans_id = UInt(32.W)
  val exception = new ExceptionEntry
}

class CsrReadWritePort extends Bundle {
  val addr = Output(UInt(12.W))
  val read_en = Output(Bool())
  val write_en = Output(Bool())
  val write_data = Output(UInt(64.W))
  val read_data = Input(UInt(64.W))
  val write_exception = Input(Bool())
}

class FuCSR extends Module {
  val io = IO(new Bundle {
    val csr_req = Flipped(Decoupled(new FuCsrReq))
    val csr_resp = Decoupled(new FuCsrResp)
    val flush = Input(Bool())

    // from commit stage, when commit a csr instruction, set csr_commit to true
    val csr_commit = Flipped(Decoupled(Bool()))
    val csr_rw_port = new CsrReadWritePort
  })
  val csr_fifo = new ValidFIFO(new FuCsrReq, 4, "csr_fifo")
  val csr_peek = csr_fifo.peek()

  val csr_fifo_pop = WireInit(false.B)
  csr_fifo.push_pop_flush_cond(
    io.csr_req.fire,
    csr_fifo_pop,
    io.flush,
    io.csr_req.bits
  )
  io.csr_req.ready := !csr_fifo.full

  def get_csr_result(
      csr_op: FuOP.Type,
      csr_rdata: UInt,
      rs_or_imm: UInt
  ): UInt = {
    assert(FuOP.is_csr(csr_op), "csr_op should be csr")
    val offset = csr_rdata(5, 0)
    val csr_result = MuxLookup(
      csr_op.asUInt,
      0.U
    )(
      Seq(
        FuOP.CSRRC.asUInt -> csr_rdata.bitSet(offset, false.B),
        FuOP.CSRRW.asUInt -> rs_or_imm,
        FuOP.CSRRS.asUInt -> csr_rdata.bitSet(offset, true.B)
      )
    )
    csr_result
  }

  private def send_csr_resp(req: FuCsrReq) = {
    io.csr_rw_port.addr := req.csr_addr
    io.csr_rw_port.read_en := true.B
    io.csr_rw_port.write_en := !req.only_read

    io.csr_resp.valid := true.B
    io.csr_resp.bits.trans_id := req.trans_id
    val csr_result = get_csr_result(
      req.csr_op,
      io.csr_rw_port.read_data,
      req.rs1_or_zimm
    )
    io.csr_rw_port.write_data := csr_result
    when(io.csr_rw_port.write_exception) {
      io.csr_resp.bits.exception.valid := true.B
      io.csr_resp.bits.data := 0.U
      io.csr_resp.bits.exception.cause := ExceptionCause.illegal_instruction
      io.csr_resp.bits.exception.tval := req.csr_addr
    }.otherwise {
      io.csr_resp.bits.exception.valid := false.B
      io.csr_resp.bits.exception.cause := ExceptionCause.unknown
      io.csr_resp.bits.exception.tval := 0.U
      io.csr_resp.bits.data := io.csr_rw_port.read_data
    }

    assert(io.csr_resp.fire, "csr_resp should be fired")

    when(io.csr_resp.fire) {
      csr_fifo_pop := true.B
    }
  }

  io.csr_resp.noenq()
  io.csr_commit.nodeq()
  io.csr_rw_port.addr := 0.U
  io.csr_rw_port.read_en := false.B
  io.csr_rw_port.write_en := false.B
  io.csr_rw_port.write_data := 0.U

  io.csr_commit.ready := csr_peek.valid
  when(io.csr_commit.fire) {
    send_csr_resp(csr_peek.bits)
  }

  // --------------------------
  // assert
  // --------------------------

  when(io.csr_commit.fire) {
    assert(!csr_fifo.empty, "csr_fifo should not be empty")
    assert(csr_peek.valid, "csr_fifo should be valid")
    assert(FuOP.is_csr(csr_peek.bits.csr_op), "csr_op should be csr")
  }
}

object gen_fu_csr_verilog extends App {
  GenVerilogHelper(new FuCSR)
}
