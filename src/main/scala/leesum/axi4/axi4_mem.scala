package leesum.axi4
import Chisel.switch
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util.{Decoupled, Enum, is}
class AXI4Memory extends Module {
  val io = IO(new AXISlaveIO(32, 64))
  io.clear()

  val ADDR_WIDTH = 12
  val DATA_WIDTH = 64
  val BASE_ADDR = 0
  ////////////////////////////
  /// internal memory
  ////////////////////////////
  val mem = Module(new BasicMemory(ADDR_WIDTH, DATA_WIDTH, BASE_ADDR))

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

  val wire_r = Wire(Decoupled((new AXIReadDataChannel(DATA_WIDTH))))
  // TODO: maybe use skid buffer
  io.r <> wire_r

  // ar_buf is used to store the ar channel
  val ar_buf = RegInit(0.U.asTypeOf(new AXIAddressChannel(32)))

  when(io.ar.fire) {
    ar_buf := io.ar.bits
  }

  // used to store the next address, used in burst modes
  val next_raddr = RegInit(0.U(32.W))

  val axi_raddr_gen = Module(new AXIAddr(ADDR_WIDTH, DATA_WIDTH))
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
  io.ar.ready := false.B

  // initial value of r channel
  wire_r.valid := false.B
  wire_r.bits := DontCare

  private def accept_ar_req(): Unit = {
    val burst_en: Bool =
      (io.ar.bits.burst === AXIDef.BURST_INCR) && (io.ar.bits.len =/= 0.U)
    io.ar.ready := true.B
    when(io.ar.fire) {
      send_internal_read_req(io.ar.bits.addr)
      when(burst_en) {
        r_state := sRBurst
        rburst_count := io.ar.bits.len
      }.otherwise({
        r_state := sRlast
        rburst_count := 0.U
      })
    }.otherwise {
      r_state := sRIdle
    }
  }

  private def send_r_resp(last: Bool): Unit = {
    wire_r.valid := true.B
    wire_r.bits.last := last
    wire_r.bits.resp := 0.U
    wire_r.bits.data := o_rdata
    wire_r.bits.id := ar_buf.id
  }

  switch(r_state) {
    is(sRIdle) {
      accept_ar_req()
      next_raddr := get_next_raddr(
        io.ar.bits.addr,
        io.ar.bits.len,
        io.ar.bits.size,
        io.ar.bits.burst
      )
    }
    is(sRBurst) {
      send_r_resp(last = false.B)
      when(wire_r.fire) {
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
      when(wire_r.fire) {
        accept_ar_req()
        next_raddr := get_next_raddr(
          io.ar.bits.addr,
          io.ar.bits.len,
          io.ar.bits.size,
          io.ar.bits.burst
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

  val wire_w = Wire(Flipped(Decoupled((new AXIWriteDataChannel(DATA_WIDTH)))))

  io.w <> wire_w

  val aw_buf = RegInit(0.U.asTypeOf(new AXIAddressChannel(32)))
  // used to store the next address, used in burst modes
  // TODO: 有问题吗？
  val next_waddr = RegInit(0.U(32.W))
  when(io.aw.fire) {
    aw_buf := io.aw.bits
    next_waddr := io.aw.bits.addr
  }

  val axi_waddr_gen = Module(new AXIAddr(ADDR_WIDTH, DATA_WIDTH))
  axi_waddr_gen.io.clear()

  def get_next_waddr(addr: UInt, len: UInt, size: UInt, burst: UInt): UInt = {
    axi_waddr_gen.io.addr := addr
    axi_waddr_gen.io.len := len
    axi_waddr_gen.io.size := size
    axi_waddr_gen.io.burst := burst
    axi_waddr_gen.io.next_addr
  }

  val b_skid_buffer = Module(
    new skid_buffer(
      new AXIWriteResponseChannel,
      CUT_VALID = true,
      CUT_READY = false
    )
  )
  b_skid_buffer.io.out <> io.b

  def accept_aw_req(): Unit = {
    // accept write address req
    io.aw.ready := true.B
    when(io.aw.fire) {
      val wburst_en: Bool =
        (io.aw.bits.burst === AXIDef.BURST_INCR) && (io.aw.bits.len =/= 0.U)
      when(wburst_en) {
        w_state := sWBurst
        wburst_count := io.aw.bits.len
      }.otherwise({
        w_state := sWlast
        wburst_count := 0.U
      })
    }.otherwise {
      w_state := sWIdle
    }
  }

  io.aw.ready := false.B
  b_skid_buffer.io.in.valid := false.B
  b_skid_buffer.io.in.bits := DontCare
  wire_w.ready := false.B

  switch(w_state) {
    is(sWIdle) {
      accept_aw_req()
    }
    is(sWBurst) {
      wire_w.ready := true.B
      when(wire_w.fire) {
        when(wburst_count === 1.U) {
          w_state := sWlast
        }.otherwise {
          w_state := sWBurst
        }
        send_internal_write_req(next_waddr, wire_w.bits.data, wire_w.bits.strb)
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
      wire_w.ready := b_skid_buffer.io.in.ready
      // wire_w.ready === true.B && b_skid_buffer.io.in.ready === true.B && wire_w.valid === true.B
      // when entry this when block, wire_w.fire === true.B, and b_skid_buffer.io.in.fire === true.B
      when(wire_w.fire) {
        b_skid_buffer.io.in.valid := true.B
        b_skid_buffer.io.in.bits.id := aw_buf.id
        b_skid_buffer.io.in.bits.resp := 0.U
        b_skid_buffer.io.in.bits.user := 0.U

        accept_aw_req()
        // 在写最后一个数据时，可以接受 aw 请求吗？
        send_internal_write_req(next_waddr, wire_w.bits.data, wire_w.bits.strb)
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
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new AXI4Memory(),
      Array(
        "--target-dir",
        verilogDir,
        "--emission-options=disableMemRandomization,disableRegisterRandomization"
      )
    )
}
