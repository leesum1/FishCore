package leesum
import chisel3._
import chisel3.util.{Decoupled, Enum, MuxLookup, is, switch}

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

class FuCSR extends Module {
  val io = IO(new Bundle {
    val csr_req = Flipped(Decoupled(new FuCsrReq))
    val csr_resp = Decoupled(new FuCsrResp)
    val flush = Input(Bool())

    // from commit stage, when commit a csr instruction, set csr_commit to true
    val csr_commit = Flipped(Decoupled(Bool()))
    val csr_read_port = new CSRReadPort
    val csr_write_port = new CSRWritePort
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

  private def send_csr_resp(exception_valid: Bool) = {
    assert(csr_peek.valid, "csr_fifo should be valid")
    assert(io.csr_commit.valid, "csr_commit should be valid")
    assert(io.csr_resp.ready, "csr_resp should be ready")

    io.csr_commit.ready := true.B
    io.csr_resp.valid := true.B
    csr_fifo_pop := true.B
    state := sIdleRead

    io.csr_resp.bits.trans_id := csr_peek.bits.trans_id
    io.csr_resp.bits.data := csr_read_buf
    io.csr_resp.bits.exception.valid := exception_valid
    io.csr_resp.bits.exception.cause := ExceptionCause.illegal_instruction
    io.csr_resp.bits.exception.tval := csr_peek.bits.csr_addr
  }

  io.csr_resp.noenq()
  io.csr_commit.nodeq()
  io.csr_read_port.addr := 0.U
  io.csr_read_port.read_en := false.B
  io.csr_write_port.addr := 0.U
  io.csr_write_port.write_en := false.B
  io.csr_write_port.write_data := 0.U

  io.csr_commit.ready := false.B

  val sIdleRead :: sWrite :: sException :: Nil = Enum(3)
  val state = RegInit(sIdleRead)
  val csr_read_buf = RegInit(0.U(64.W))

  switch(state) {
    is(sIdleRead) {
      when(csr_peek.valid && io.csr_commit.valid) {
        io.csr_read_port.addr := csr_peek.bits.csr_addr
        io.csr_read_port.read_en := true.B
        csr_read_buf := io.csr_read_port.read_data

        when(io.csr_read_port.read_ex_resp) {
          state := sException
        }.otherwise {
          state := sWrite
        }
      }
    }
    is(sWrite) {
      val csr_wdata = get_csr_result(
        csr_peek.bits.csr_op,
        csr_read_buf,
        csr_peek.bits.rs1_or_zimm
      )
      io.csr_write_port.addr := csr_peek.bits.csr_addr
      io.csr_write_port.write_en := !csr_peek.bits.only_read
      io.csr_write_port.write_data := csr_wdata
      when(io.csr_write_port.write_ex_resp && !csr_peek.bits.only_read) {
        send_csr_resp(true.B)
      }.otherwise {
        send_csr_resp(false.B)
      }
    }
    is(sException) {
      send_csr_resp(true.B)
    }

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
