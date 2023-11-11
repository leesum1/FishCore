package leesum

import chisel3._
import chisel3.util.{Decoupled, Enum, MuxLookup, is, switch}
object AMOOP extends ChiselEnum {
  val None = Value(0.U)
  val ADD = Value(1.U)
  val AND = Value(2.U)
  val MAX = Value(3.U)
  val MAXU = Value(4.U)
  val MIN = Value(5.U)
  val MINU = Value(6.U)
  val OR = Value(7.U)
  val SWAP = Value(8.U)
  val XOR = Value(9.U)
  val LR = Value(10.U)
  val SC = Value(11.U)

  def isCmp(amo_op: AMOOP.Type): Bool = {
    amo_op === AMOOP.MAX || amo_op === AMOOP.MAXU || amo_op === AMOOP.MIN || amo_op === AMOOP.MINU
  }

  def FuOP2AMOOP(fu_op: FuOP.Type): AMOOP.Type = {
    val amoop = Wire(AMOOP())
    amoop := MuxLookup(fu_op.asUInt, AMOOP.None)(
      Seq(
        FuOP.LsuAMOADD.asUInt -> AMOOP.ADD,
        FuOP.LsuAMOAND.asUInt -> AMOOP.AND,
        FuOP.LsuAMOMAX.asUInt -> AMOOP.MAX,
        FuOP.LsuAMOMAXU.asUInt -> AMOOP.MAXU,
        FuOP.LsuAMOMIN.asUInt -> AMOOP.MIN,
        FuOP.LsuAMOMINU.asUInt -> AMOOP.MINU,
        FuOP.LsuAMOOR.asUInt -> AMOOP.OR,
        FuOP.LsuAMOSWAP.asUInt -> AMOOP.SWAP,
        FuOP.LsuAMOXOR.asUInt -> AMOOP.XOR,
        FuOP.LsuLR.asUInt -> AMOOP.LR,
        FuOP.LsuSC.asUInt -> AMOOP.SC
      )
    )
    amoop
  }
}
class AMOQueueIn extends Bundle {
  val rs1_data = UInt(64.W)
  val rs2_data = UInt(64.W)
  val is_rv32 = Bool()
  val amo_op = AMOOP()
  val trans_id = UInt(32.W)
}
class AMOQueue extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AMOQueueIn))
    val flush = Input(Bool())
    val store_queue_empty = Input(Bool())
    val amo_commit = Flipped(Decoupled(Bool()))

    val amo_writeback = Decoupled(new LSUResp)
    val load_req = Decoupled(new LoadDcacheReq)
    val load_resp = Flipped(Decoupled(new LoadDcacheResp))
    val store_req = Decoupled(new StoreDcacheReq)
    val store_resp = Flipped(Decoupled(new StoreDcacheResp))
  })

  val amo_queue = PipeLine(io.in, io.flush)

  val sIdle :: sWaitLoadResp :: sWaitStoreReq :: sWaitStoreResp :: Nil =
    Enum(4)

  val state = RegInit(sIdle)

  val lr_sc_reservation = RegInit(0.U(64.W))

  val is_sc = amo_queue.bits.amo_op === AMOOP.SC
  val is_lr = amo_queue.bits.amo_op === AMOOP.LR
  val reservation_ok =
    is_sc && lr_sc_reservation === amo_queue.bits.rs1_data

  amo_queue.nodeq()
  io.amo_commit.ready := false.B
  io.load_req.noenq()
  io.load_resp.nodeq()
  io.store_req.noenq()
  io.store_resp.nodeq()
  io.amo_writeback.noenq()

  def get_amo_result(amo_req: AMOQueueIn, mem_data: UInt) = {
    val result = Wire(UInt(64.W))
    val adder = Module(new AluAdder())
    adder.io.sub_req := AMOOP.isCmp(amo_req.amo_op)
    adder.io.adder_in1 := Mux(
      amo_req.is_rv32,
      SignExt(mem_data(31, 0), 32, 64, true.B),
      mem_data
    )
    adder.io.adder_in2 := Mux(
      amo_req.is_rv32,
      SignExt(amo_req.rs2_data(31, 0), 32, 64, true.B),
      amo_req.rs2_data
    )

    result := MuxLookup(amo_req.amo_op.asUInt, 0.U) {
      Seq(
        AMOOP.ADD.asUInt -> adder.io.adder_out,
        AMOOP.AND.asUInt -> (mem_data & amo_req.rs2_data),
        AMOOP.OR.asUInt -> (mem_data | amo_req.rs2_data),
        AMOOP.XOR.asUInt -> (mem_data ^ amo_req.rs2_data),
        AMOOP.SWAP.asUInt -> amo_req.rs2_data,
        AMOOP.MAX.asUInt -> Mux(adder.io.slt, amo_req.rs2_data, mem_data),
        AMOOP.MAXU.asUInt -> Mux(adder.io.sltu, amo_req.rs2_data, mem_data),
        AMOOP.MIN.asUInt -> Mux(adder.io.slt, mem_data, amo_req.rs2_data),
        AMOOP.MINU.asUInt -> Mux(adder.io.sltu, mem_data, amo_req.rs2_data),
        AMOOP.SC.asUInt -> mem_data
      )
    }

    result
  }

  def send_load_req() = {
    when(amo_queue.valid && io.store_queue_empty && !io.flush) {
      assert(
        !is_sc,
        "amo op fail"
      )

      io.load_req.valid := true.B
      // TODO: not implemented
      io.load_req.bits.is_mmio := false.B
      io.load_req.bits.paddr := amo_queue.bits.rs1_data
      io.load_req.bits.size := Mux(
        amo_queue.bits.is_rv32,
        2.U,
        3.U
      ) // 4 or 8 bytes
      when(io.load_req.fire) {
        // LOAD RESERVATION
        when(amo_queue.bits.amo_op === AMOOP.LR) {
          lr_sc_reservation := amo_queue.bits.rs1_data
        }
        state := sWaitLoadResp
      }
    }.otherwise {
      state := sIdle
    }
  }

  def send_store_req() = {

    when(amo_queue.valid && io.store_queue_empty && !io.flush) {
      assert(!is_lr, "amo op fail")

      io.store_req.valid := Mux(is_sc, reservation_ok, true.B)
      // TODO: not implemented
      io.store_req.bits.is_mmio := false.B
      io.store_req.bits.paddr := amo_queue.bits.rs1_data
      io.store_req.bits.size := Mux(
        amo_queue.bits.is_rv32,
        2.U,
        3.U
      ) // 4 or 8 bytes
      io.store_req.bits.wstrb := GenAxiWstrb(
        amo_queue.bits.rs1_data,
        io.store_req.bits.size
      )
      io.store_req.bits.wdata := GenAxiWdata(
        get_amo_result(
          amo_queue.bits,
          Mux(is_sc, amo_queue.bits.rs2_data, load_shifted_rdata)
        ),
        amo_queue.bits.rs1_data
      )

      // SC
      when(is_sc && !reservation_ok) {
        // sc fail, do not send store req, send writeback 1
        assert(
          !io.store_req.valid,
          " should not send store req when reservation fail"
        )
        lr_sc_reservation := 0.U
        send_amo_writeback(1.U)
      }.elsewhen(io.store_req.fire) {

        state := sWaitStoreResp
      }
    }
  }

  def send_amo_writeback(data: UInt): Unit = {
    io.amo_writeback.valid := true.B
    io.amo_writeback.bits.wb_data := Mux(
      amo_queue.bits.is_rv32,
      SignExt(data(31, 0), 32, 64, true.B),
      data
    )
    io.amo_writeback.bits.trans_id := amo_queue.bits.trans_id

    io.amo_commit.ready := true.B
    amo_queue.ready := true.B

    assert(amo_queue.fire, "amo_queue should be fire")
    assert(io.amo_commit.fire, "amo_commit should be fire")
    state := sIdle
  }

  val load_shifted_rdata = RegInit(0.U(64.W))

  // TODO: LR SC not implemented
  switch(state) {
    is(sIdle) {
      when(io.amo_commit.valid) {
        when(is_sc) {
          send_store_req()
        }.otherwise {
          send_load_req()
        }
      }
    }
    is(sWaitLoadResp) {
      io.load_resp.ready := true.B
      when(io.load_resp.fire) {
        when(amo_queue.bits.amo_op === AMOOP.LR) {
          send_amo_writeback(io.load_resp.bits.data)
        }.otherwise {
          // the lsb of rdata is in the lsb of load_shifted_rdata
          load_shifted_rdata := GetAxiRdata(
            io.load_resp.bits.data,
            amo_queue.bits.rs1_data,
            Mux(
              amo_queue.bits.is_rv32,
              2.U, // 4 bytes
              3.U // 8 bytes
            ),
            amo_queue.bits.is_rv32
          )
          state := sWaitStoreReq
        }
      }
    }
    is(sWaitStoreReq) {
      assert(!is_sc, "amo op fail")
      send_store_req()
    }

    is(sWaitStoreResp) {
      io.store_resp.ready := true.B
      when(io.store_resp.fire) {

        when(is_sc) {
          lr_sc_reservation := 0.U
          assert(reservation_ok, "reservation must be ok")
        }
        send_amo_writeback(Mux(is_sc, 0.U, load_shifted_rdata)) // sc fail
      }
    }
  }

  // ----------------------
  // assert
  // ----------------------
  when(io.amo_writeback.valid) {
    assert(io.amo_writeback.fire, "amo_writeback should be fire")
  }
}

object gen_amo_queue_verilog extends App {
  GenVerilogHelper(new AMOQueue)
}