package leesum
import chisel3._
import chisel3.util.{Decoupled, Enum, MuxLookup, is, switch}

class FuCsrReq extends Bundle {
  val csr_addr = UInt(12.W)
  val rs1_or_zimm = UInt(64.W) // zimm or rs1_data
  val read_en = Bool()
  val write_en = Bool()
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
//  val csr_fifo = new ValidFIFO(new FuCsrReq, 4, "csr_fifo")
  val csr_fifo = new MultiPortFIFOBase(
    new FuCsrReq,
    4,
    1,
    1,
    use_mem = true,
    with_valid = false
  )

  val csr_peek = csr_fifo.peek().head

  val csr_fifo_pop = WireInit(false.B)
  csr_fifo.push_pop_flush_cond(
    io.csr_req.fire.asBools,
    csr_fifo_pop.asBools,
    io.flush,
    Seq(io.csr_req.bits)
  )
  io.csr_req.ready := !csr_fifo.full

  def get_csr_result(
      csr_op: FuOP.Type,
      csr_rdata: UInt,
      rs_or_imm: UInt
  ): UInt = {
    assert(FuOP.is_csr(csr_op), "csr_op should be csr")
    require(csr_rdata.getWidth == rs_or_imm.getWidth, "width should be equal")
    val csr_result = MuxLookup(
      csr_op.asUInt,
      0.U
    )(
      Seq(
        FuOP.CSRRC.asUInt -> (csr_rdata & (~rs_or_imm).asUInt),
        FuOP.CSRRW.asUInt -> rs_or_imm,
        FuOP.CSRRS.asUInt -> (csr_rdata | rs_or_imm)
      )
    )
    csr_result
  }

  private def send_csr_resp(exception_valid: Bool) = {
    assert(csr_peek.valid, "csr_fifo should be valid")
    assert(io.csr_commit.valid, "csr_commit should be valid")
    assert(io.csr_resp.ready, "csr_resp should be ready")

    io.csr_commit.ready := true.B && !io.flush
    io.csr_resp.valid := true.B
    csr_fifo_pop := true.B
    state := sIdleRead

    io.csr_resp.bits.trans_id := csr_peek.bits.trans_id
    io.csr_resp.bits.data := csr_read_buf
    io.csr_resp.bits.exception.valid := exception_valid
    io.csr_resp.bits.exception.cause := ExceptionCause.illegal_instruction
    // TODO: shall we set tval to csr_addr?
    //    io.csr_resp.bits.exception.tval := csr_peek.bits.csr_addr
    io.csr_resp.bits.exception.tval := 0.U
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

  // TODO: need check carefully
  switch(state) {
    is(sIdleRead) {
      when(csr_peek.valid && io.csr_commit.valid && !io.flush) {
        io.csr_read_port.addr := csr_peek.bits.csr_addr
        io.csr_read_port.read_en := csr_peek.bits.read_en
        csr_read_buf := Mux(
          csr_peek.bits.read_en,
          io.csr_read_port.read_data,
          0.U
        )

        when(io.csr_read_port.read_ex_resp) {
          state := sException
        }.otherwise {
          state := sWrite
        }
      }.otherwise {
        state := sIdleRead
      }
    }
    is(sWrite) {

      when(!io.flush) {
        val csr_wdata = get_csr_result(
          csr_peek.bits.csr_op,
          csr_read_buf,
          csr_peek.bits.rs1_or_zimm
        )
        io.csr_write_port.addr := csr_peek.bits.csr_addr
        io.csr_write_port.write_en := csr_peek.bits.write_en
        io.csr_write_port.write_data := csr_wdata
        when(io.csr_write_port.write_ex_resp) {
          send_csr_resp(true.B)
        }.otherwise {
          send_csr_resp(false.B)
        }
      }.otherwise {
        state := sIdleRead
      }

    }
    is(sException) {
      when(!io.flush) {
        send_csr_resp(true.B)
      }.otherwise {
        state := sIdleRead
      }
    }
  }

  // --------------------------
  // assert
  // --------------------------

  when(io.csr_commit.fire) {
    assert(!csr_fifo.empty, "csr_fifo should not be empty")
    assert(csr_peek.valid, "csr_fifo should be valid")
    assert(FuOP.is_csr(csr_peek.bits.csr_op), "csr_op should be csr")
    assert(
      csr_peek.bits.read_en | csr_peek.bits.write_en,
      "csr should be read or write"
    )
  }
}

object gen_fu_csr_verilog extends App {
  GenVerilogHelper(new FuCSR)
}
