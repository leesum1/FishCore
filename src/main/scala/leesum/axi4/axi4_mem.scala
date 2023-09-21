package leesum.axi4
import chisel3._
import chisel3.util.{Decoupled, Enum, is, isPow2, switch}
import circt.stage.ChiselStage
import leesum.GenVerilogHelper

import scala.math.BigInt.{int2bigInt, long2bigInt}
class AXI4Memory(
    AXI_AW: Int, // axi address width
    AXI_DW: Int, // axi data width
    INTERNAL_MEM_SIZE: Long, // internal memory size
    INTERNAL_MEM_DW: Int, // internal memory data width
    INTERNAL_MEM_BASE: Long, // internal memory base address,
    memoryFile: String = ""
) extends Module {
  require(AXI_DW == 32 || AXI_DW == 64, "AXI_DW must be 32 or 64")
  require(AXI_AW == 32 || AXI_AW == 64, "AXI_AW must be 32 or 64")
  require(AXI_DW == INTERNAL_MEM_DW, "AXI_DW must be equal to INTERNAL_MEM_DW")
  require(
    isPow2(INTERNAL_MEM_SIZE),
    "INTERNAL_MEM_SIZE must be power of 2"
  )
  require(INTERNAL_MEM_BASE >= 0, "INTERNAL_MEM_BASE must be positive")
  require(
    INTERNAL_MEM_BASE % INTERNAL_MEM_SIZE == 0,
    "INTERNAL_MEM_BASE must be multiple of INTERNAL_MEM_SIZE"
  )

  val io = IO(new AXISlaveIO(AXI_AW, AXI_DW))
  io.clear()
  ///////////////////////////////
  /// register all output signals
  ///////////////////////////////

  val axi_ar = Wire(Decoupled(new AXIAddressChannel(AXI_AW)))
  skid_buffer(
    io.ar,
    axi_ar,
    CUT_VALID = false,
    CUT_READY = true
  )
  val axi_aw = Wire(Decoupled(new AXIAddressChannel(AXI_AW)))

  skid_buffer(
    io.aw,
    axi_aw,
    CUT_VALID = false,
    CUT_READY = true
  )
  val axi_w = Wire(Decoupled(new AXIWriteDataChannel(AXI_DW)))
  skid_buffer(
    io.w,
    axi_w,
    CUT_VALID = false,
    CUT_READY = true
  )

  val axi_b = Wire(Decoupled(new AXIWriteResponseChannel))
  skid_buffer(
    axi_b,
    io.b,
    CUT_VALID = true,
    CUT_READY = false
  )
  val axi_r = Wire(Decoupled(new AXIReadDataChannel(AXI_DW)))
  skid_buffer(
    axi_r,
    io.r,
    CUT_VALID = true,
    CUT_READY = false
  )

  val ADDR_WIDTH = (INTERNAL_MEM_BASE + INTERNAL_MEM_SIZE).bitLength
  val DATA_WIDTH = INTERNAL_MEM_DW
  val BASE_ADDR = INTERNAL_MEM_BASE
  ////////////////////////////
  /// internal memory
  ////////////////////////////
  val mem = Module(
    new BasicMemory(ADDR_WIDTH, DATA_WIDTH, BASE_ADDR, memoryFile)
  )

  val x = WireInit
  val i_we = WireInit(Bool(), false.B)
  val i_wstrb = WireInit(UInt((DATA_WIDTH / 8).W), 0.U)
  val i_waddr = WireInit(UInt(ADDR_WIDTH.W), 0.U)
  val i_wdata = WireInit(UInt(DATA_WIDTH.W), 0.U)
  val i_rd = WireInit(Bool(), false.B)
  val i_raddr = WireInit(UInt(ADDR_WIDTH.W), 0.U)
  val o_rdata = WireInit(UInt(DATA_WIDTH.W), 0.U)
  mem.io.i_rd := i_rd
  mem.io.i_raddr := i_raddr
  mem.io.i_we := i_we
  mem.io.i_waddr := i_waddr
  mem.io.i_wdata := i_wdata
  mem.io.i_wstrb := i_wstrb
  o_rdata := mem.io.o_rdata

  def send_internal_read_req(addr: UInt) = {
    i_rd := true.B
    i_raddr := addr
  }
  def send_internal_write_req(addr: UInt, data: UInt, strb: UInt) = {
    i_we := true.B
    i_waddr := addr
    i_wdata := data
    i_wstrb := strb
  }

  ////////////////////////////
  /// read state machine
  ////////////////////////////
  val sRIdle :: sRBurst :: sRlast :: Nil = Enum(3)
  val r_state = RegInit(sRIdle)
  val rburst_count = RegInit(0.U(8.W))

  // ar_buf is used to store the ar channel
  val ar_buf = RegInit(0.U.asTypeOf(new AXIAddressChannel(32)))

  when(axi_ar.fire) {
    ar_buf := axi_ar.bits
  }

  // used to store the next address, used in burst modes
  val next_raddr = RegInit(0.U(32.W))

  val axi_raddr_gen = Module(new AXIAddr(AXI_AW, AXI_DW))
  axi_raddr_gen.io.clear()

  def get_next_raddr(addr: UInt, len: UInt, size: UInt, burst: UInt): UInt = {
    axi_raddr_gen.io.addr := addr
    axi_raddr_gen.io.len := len
    axi_raddr_gen.io.size := size
    axi_raddr_gen.io.burst := burst
    axi_raddr_gen.io.next_addr
  }

  // initial value of internal memory
  i_we := false.B
  i_waddr := 0.U
  i_wdata := 0.U
  i_wstrb := 0.U
  i_rd := false.B
  i_raddr := 0.U

  // initial value of ar channel
  axi_ar.ready := false.B

  // initial value of r channel
  axi_r.valid := false.B
  axi_r.bits := DontCare

  private def accept_ar_req(): Unit = {
    val burst_en: Bool =
      (axi_ar.bits.burst === AXIDef.BURST_INCR) && (axi_ar.bits.len =/= 0.U)
    axi_ar.ready := true.B
    when(axi_ar.fire) {
      send_internal_read_req(axi_ar.bits.addr)
      when(burst_en) {
        r_state := sRBurst
        rburst_count := axi_ar.bits.len
      }.otherwise({
        r_state := sRlast
        rburst_count := 0.U
      })
    }.otherwise {
      r_state := sRIdle
    }
  }

  private def send_r_resp(last: Bool): Unit = {
    axi_r.valid := true.B
    axi_r.bits.last := last
    axi_r.bits.resp := 0.U
    axi_r.bits.data := o_rdata
    axi_r.bits.id := ar_buf.id
  }

  switch(r_state) {
    is(sRIdle) {
      accept_ar_req()
      next_raddr := get_next_raddr(
        axi_ar.bits.addr,
        axi_ar.bits.len,
        axi_ar.bits.size,
        axi_ar.bits.burst
      )
    }
    is(sRBurst) {
      send_r_resp(last = false.B)
      when(axi_r.fire) {
        when(rburst_count === 1.U) {
          r_state := sRlast
        }.otherwise {
          r_state := sRBurst
        }

        send_internal_read_req(next_raddr)
        next_raddr := get_next_raddr(
          next_raddr,
          ar_buf.len,
          ar_buf.size,
          ar_buf.burst
        )
        rburst_count := rburst_count - 1.U
      }
    }
    is(sRlast) {
      send_r_resp(last = true.B)
      when(axi_r.fire) {
        accept_ar_req()
        next_raddr := get_next_raddr(
          axi_ar.bits.addr,
          axi_ar.bits.len,
          axi_ar.bits.size,
          axi_ar.bits.burst
        )
      }
    }
  }

  ////////////////////////////
  /// write state machine
  ////////////////////////////
  val sWIdle :: sWBurst :: sWlast :: Nil = Enum(3)
  val w_state = RegInit(sWIdle)
  val wburst_count = RegInit(0.U(8.W))

  val aw_buf = RegInit(0.U.asTypeOf(new AXIAddressChannel(AXI_AW)))
  // used to store the next address, used in burst modes
  // TODO: 有问题吗？
  val next_waddr = RegInit(0.U(AXI_AW.W))
  when(axi_aw.fire) {
    aw_buf := axi_aw.bits
    next_waddr := axi_aw.bits.addr
  }

  val axi_waddr_gen = Module(new AXIAddr(AXI_AW, AXI_DW))
  axi_waddr_gen.io.clear()

  def get_next_waddr(addr: UInt, len: UInt, size: UInt, burst: UInt): UInt = {
    axi_waddr_gen.io.addr := addr
    axi_waddr_gen.io.len := len
    axi_waddr_gen.io.size := size
    axi_waddr_gen.io.burst := burst
    axi_waddr_gen.io.next_addr
  }

  def accept_aw_req(): Unit = {
    // accept write address req
    axi_aw.ready := true.B
    when(axi_aw.fire) {
      val wburst_en: Bool =
        (axi_aw.bits.burst === AXIDef.BURST_INCR) && (axi_aw.bits.len =/= 0.U)
      when(wburst_en) {
        w_state := sWBurst
        wburst_count := axi_aw.bits.len
      }.otherwise({
        w_state := sWlast
        wburst_count := 0.U
      })
    }.otherwise {
      w_state := sWIdle
    }
  }

  axi_aw.ready := false.B
  axi_b.valid := false.B
  axi_b.bits := DontCare
  axi_w.ready := false.B

  switch(w_state) {
    is(sWIdle) {
      accept_aw_req()
    }
    is(sWBurst) {
      axi_w.ready := true.B
      when(axi_w.fire) {
        when(wburst_count === 1.U) {
          w_state := sWlast
        }.otherwise {
          w_state := sWBurst
        }
        send_internal_write_req(next_waddr, axi_w.bits.data, axi_w.bits.strb)
        next_waddr := get_next_waddr(
          next_waddr,
          aw_buf.len,
          aw_buf.size,
          aw_buf.burst
        )
        wburst_count := wburst_count - 1.U
      }
    }
    is(sWlast) {
      axi_w.ready := axi_b.ready
      // axi_w.ready === true.B && b_skid_buffer.io.in.ready === true.B && axi_w.valid === true.B
      // when entry this when block, axi_w.fire === true.B, and b_skid_buffer.io.in.fire === true.B
      when(axi_w.fire) {
        axi_b.valid := true.B
        axi_b.bits.id := aw_buf.id
        axi_b.bits.resp := 0.U
        axi_b.bits.user := 0.U

        accept_aw_req()
        // 在写最后一个数据时，可以接受 aw 请求吗？
        send_internal_write_req(next_waddr, axi_w.bits.data, axi_w.bits.strb)
      }
    }
  }

}

class fsm_test extends Module {
  val io = IO(new Bundle {
    val in_valid = Input(Bool())
    val in_data = Input(UInt(4.W))
    val wire_out = Output(UInt(4.W))
    val reg_out = Output(UInt(4.W))
  })

  val sIdle :: sFirst :: Nil = Enum(2)
  val state = RegInit(sIdle)
  val wire_a = WireInit(0.U(4.W))
  val reg_a = RegInit(0.U(4.W))

  // before switch, wire_a := 0.U, reg_a := 0.U
  reg_a := 0.U
  wire_a := 0.U
  switch(state) {
    is(sIdle) {
      when(io.in_valid) {
        state := sFirst
        wire_a := io.in_data + 1.U
        reg_a := io.in_data + 1.U
      }
    }
    is(sFirst) {
      when(io.in_valid) {
        state := sIdle
        wire_a := io.in_data + 2.U
        reg_a := io.in_data + 2.U
      }
    }
  }
  io.wire_out := wire_a
  io.reg_out := reg_a
}
object gen_verilog extends App {
  GenVerilogHelper(
    new AXI4Memory(
      AXI_AW = 64,
      AXI_DW = 32,
      INTERNAL_MEM_SIZE = 0x10000,
      INTERNAL_MEM_DW = 32,
      INTERNAL_MEM_BASE = 0
    )
  )

}
