package leesum
import chisel3._
import chisel3.util.{
  Decoupled,
  Enum,
  MuxCase,
  MuxLookup,
  PriorityMux,
  Queue,
  is,
  switch
}
import leesum.axi4.StreamFork2

class AGUReq extends Bundle {
  val op_a = UInt(64.W)
  val op_b = UInt(64.W)
  val size = UInt(2.W)
  val store_data = UInt(64.W)
  val trans_id = UInt(32.W)
  val is_store = Bool()
  val amo_op = AMOOP()
  // need by load
  val sign_ext = Bool()
}

class AGUWriteBack extends Bundle {
  val trans_id = UInt(32.W)
  val is_mmio = Bool()
  val is_store = Bool()
  val exception = new ExceptionEntry()
}

class AGUResp extends Bundle {
  val load_pipe = Decoupled(new LoadQueueIn())
  val store_pipe = Decoupled(new StoreQueueIn())
  val amo_pipe = Decoupled(new AMOQueueIn())
  val agu_pipe = Decoupled(new AGUWriteBack())
}

class AGU(
    // ADDR_START, ADDR_END, mmio
    addr_map: Seq[(Long, Long, Boolean)] = Seq(
      (
        0,
        0xffffffffffffffffL,
        false
      ) // default addr map, 0x0 -> 0xffffffffffffffff, mmio = false
    )
) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AGUReq))
    val out = new AGUResp
    val flush = Input(Bool())
    // tlb interface
    val tlb_req = Decoupled(new TLBReq)
    val tlb_resp = Flipped(Decoupled(new TLBResp))
    // from store bypass
    val store_bypass = Flipped(new StoreBypassIO)
  })

//  val agu_req = Wire(Flipped(Decoupled(new AGUReq)))

  val agu_req = Queue(io.in, 4, flow = true, flush = Some(io.flush))

//  SkidBufferWithFLush(
//    io.in,
//    agu_req,
//    io.flush,
//    CUT_VALID = false,
//    CUT_READY = true
//  )

  // TODO: need optimize, use alu to calculate vaddr?
  val vaddr = agu_req.bits.op_a.asSInt + agu_req.bits.op_b.asSInt

  val vaddr_buf = RegInit(0.U(64.W))

  val sIdle :: sWaitTLBRsp :: sWaitFifo :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val tlb_resp_buf = RegInit(0.U.asTypeOf(new TLBResp))
  val agu_req_buf = RegInit(0.U.asTypeOf(new AGUReq))

  io.tlb_req.noenq()
  io.tlb_resp.nodeq()
  agu_req.nodeq()

  io.out.agu_pipe.noenq()
  io.out.store_pipe.noenq()
  io.out.load_pipe.noenq()
  io.out.amo_pipe.noenq()
  io.store_bypass.valid := false.B
  io.store_bypass.paddr := DontCare

  /** when tlb ready, accept the request from agu
    */
  def send_tlb_req(): Unit = {
    val is_amo = agu_req.bits.amo_op =/= AMOOP.None

    assert(
      !(agu_req.bits.is_store && is_amo),
      "send_tlb_req error, is_store and is_amo should not be true at the same time"
    )

    agu_req.ready := io.tlb_req.ready && !io.flush
    io.tlb_req.valid := agu_req.valid
    io.tlb_req.bits.vaddr := vaddr.asUInt
    io.tlb_req.bits.size := agu_req.bits.size
    io.tlb_req.bits.req_type := MuxCase(
      TLBReqType.LOAD,
      Seq(
        agu_req.bits.is_store -> TLBReqType.STORE,
        (agu_req.bits.amo_op =/= AMOOP.None) -> TLBReqType.AMO
      )
    )

    when(agu_req.fire) {
      assert(!io.flush)
      assert(io.tlb_req.fire)
      state := sWaitTLBRsp
      agu_req_buf := agu_req.bits
      vaddr_buf := vaddr.asUInt
    }.otherwise {
      // flush or not fire
      state := sIdle
    }
  }

  def addr_in_range(addr: UInt): Bool = {
    val in_range = addr_map
      .map { case (start, end, _) =>
        addr >= Long2UInt64(start) && addr <= Long2UInt64(end)
      }
      .reduce(_ || _)

    in_range
  }

  // TODO: not implemented now
  def check_privilege(tlb_rsp: TLBResp): Bool = {
    val addr_align = CheckAligned(tlb_rsp.paddr, tlb_rsp.size)
    val in_range = addr_in_range(tlb_rsp.paddr)
    Seq(tlb_rsp.exception.valid, !addr_align, !in_range).reduce(_ || _)
  }

  def check_mmio(tlb_rsp: TLBResp): Bool = {

    val is_mmio = WireInit(false.B)
    addr_map
      .foreach { case (start, end, ismmio) =>
        when(
          tlb_rsp.paddr >= Long2UInt64(start) && tlb_rsp.paddr < Long2UInt64(
            end
          )
        ) {
          is_mmio := ismmio.B
        }
      }
    is_mmio
  }

  def dispatch_to_exception(tlb_rsp: TLBResp): Unit = {

    io.out.agu_pipe.valid := true.B
    when(io.out.agu_pipe.fire) {
      io.out.agu_pipe.bits.exception.valid := true.B
      io.out.agu_pipe.bits.trans_id := agu_req_buf.trans_id

      // tlb_rsp.paddr only valid when tlb_rsp.exception.valid is false
      val is_misaligned =
        !CheckAligned(tlb_rsp.paddr, tlb_rsp.size) && !tlb_rsp.exception.valid
      val out_of_range =
        !addr_in_range(tlb_rsp.paddr) && !tlb_rsp.exception.valid
      // priority order: high -> low
      // 1. tlb exception
      // 2. out of range
      // 3. misaligned
      // 4. defeat unknown
      io.out.agu_pipe.bits.exception.cause := PriorityMux(
        Seq(
          tlb_rsp.exception.valid -> tlb_rsp.exception.cause,
          out_of_range -> ExceptionCause.get_access_cause(tlb_rsp.req_type),
          is_misaligned -> ExceptionCause.get_misaligned_cause(
            tlb_rsp.req_type
          ),
          true.B -> ExceptionCause.unknown
        )
      )
      // If mtval is written with a nonzero value when a misaligned load or store causes an access-fault or
      // page-fault exception, then mtval will contain the virtual address of the portion of the access that
      // caused the fault.
      io.out.agu_pipe.bits.exception.tval := vaddr_buf

      // back to back
      send_tlb_req()
    }.otherwise {
      state := sWaitFifo
    }
  }

  def dispatch_to_load(tlb_rsp: TLBResp): Unit = {
    // when current load addr conflict with uncommitted store addr, and the store is a mmio operation
    // then stall the current load.
    val need_stall =
      io.store_bypass.data.valid && io.store_bypass.data.is_mmio || !io.out.amo_pipe.ready
    val is_mmio = check_mmio(tlb_rsp)

    val load_fork_in = Wire(Decoupled(Bool()))
    load_fork_in.valid := !need_stall && tlb_rsp.req_type === TLBReqType.LOAD
    load_fork_in.bits := DontCare // not used

    // MUST USE SYNC FORK
    val (load_fork_out1, load_fork_out2) = StreamFork2(load_fork_in, true)

    load_fork_out1.ready := io.out.load_pipe.ready
    load_fork_out2.ready := Mux(is_mmio, io.out.agu_pipe.ready, true.B)
    // -----------------------
    // load pipe fork
    // -----------------------
    io.out.load_pipe.valid := load_fork_out1.valid
    io.out.load_pipe.bits.paddr := tlb_rsp.paddr
    io.out.load_pipe.bits.size := agu_req_buf.size
    io.out.load_pipe.bits.trans_id := agu_req_buf.trans_id
    io.out.load_pipe.bits.store_bypass := Mux(
      io.store_bypass.data.valid,
      io.store_bypass.data,
      0.U.asTypeOf(new StoreBypassData)
    )
    io.out.load_pipe.bits.sign_ext := agu_req_buf.sign_ext
    io.out.load_pipe.bits.is_mmio := is_mmio
    // -----------------------
    // exception pipe fork
    // -----------------------
    // if the load is mmio, then write back
    io.out.agu_pipe.valid := load_fork_out2.valid && is_mmio
    io.out.agu_pipe.bits.is_mmio := is_mmio
    io.out.agu_pipe.bits.is_store := false.B
    io.out.agu_pipe.bits.trans_id := agu_req_buf.trans_id
    io.out.agu_pipe.bits.exception.valid := false.B
    io.out.agu_pipe.bits.exception.cause := ExceptionCause.unknown
    io.out.agu_pipe.bits.exception.tval := 0.U

    when(load_fork_in.fire) {
      // back to back
      send_tlb_req()
    }.otherwise {
      state := sWaitFifo
    }
  }

  def dispatch_to_store(tlb_rsp: TLBResp): Unit = {
    // when current store addr conflict with uncommitted store addr.
    // than stall dispatch the current store

    val need_stall = io.store_bypass.data.valid || !io.out.amo_pipe.ready
    val is_mmio = check_mmio(tlb_rsp)

    val store_fork_in = Wire(Decoupled(Bool()))
    store_fork_in.valid := !need_stall && tlb_rsp.req_type === TLBReqType.STORE
    store_fork_in.bits := DontCare // not used

    // MUST USE SYNC FORK
    val (store_fork_out1, store_fork_out2) = StreamFork2(store_fork_in, true)
    store_fork_out1.ready := io.out.store_pipe.ready
    store_fork_out2.ready := io.out.agu_pipe.ready

    // -----------------------
    // store pipe fork
    // -----------------------
    io.out.store_pipe.valid := store_fork_out1.valid
    io.out.store_pipe.bits.paddr := tlb_rsp.paddr
    io.out.store_pipe.bits.size := agu_req_buf.size
    // convert store_data to axi wdata
    io.out.store_pipe.bits.wdata := GenAxiWdata(
      agu_req_buf.store_data,
      tlb_rsp.paddr
    )
    io.out.store_pipe.bits.wstrb := GenAxiWstrb(
      tlb_rsp.paddr,
      agu_req_buf.size
    )
    io.out.store_pipe.bits.trans_id := agu_req_buf.trans_id
    io.out.store_pipe.bits.is_mmio := is_mmio
    // --------------------------
    // exception pipe fork
    // --------------------------
    io.out.agu_pipe.valid := store_fork_out2.valid
    io.out.agu_pipe.bits.is_mmio := is_mmio
    io.out.agu_pipe.bits.is_store := true.B
    io.out.agu_pipe.bits.trans_id := agu_req_buf.trans_id
    io.out.agu_pipe.bits.exception.valid := false.B
    io.out.agu_pipe.bits.exception.cause := ExceptionCause.unknown
    io.out.agu_pipe.bits.exception.tval := 0.U

    when(store_fork_in.fire) {
      // back to back
      send_tlb_req()
    }.otherwise {
      state := sWaitFifo
    }
  }

  def dispatch_to_amo(tlb_resp: TLBResp) = {
    io.out.amo_pipe.valid := true.B
    io.out.amo_pipe.bits.trans_id := agu_req_buf.trans_id
    io.out.amo_pipe.bits.rs1_data := agu_req_buf.op_a
    io.out.amo_pipe.bits.rs2_data := agu_req_buf.store_data // store_data is rs2_data
    io.out.amo_pipe.bits.is_rv32 := agu_req_buf.size === DcacheConst.SIZE4.asUInt
    io.out.amo_pipe.bits.amo_op := agu_req_buf.amo_op

    when(io.out.amo_pipe.fire) {
      assert(
        !agu_req_buf.is_store,
        "dispatch_to_amo error, should not be store"
      )
      assert(
        agu_req_buf.amo_op =/= AMOOP.None,
        "dispatch_to_amo error, amo_op should not be None"
      )
      assert(
        agu_req_buf.size === DcacheConst.SIZE8.asUInt || agu_req_buf.size === DcacheConst.SIZE4.asUInt,
        "dispatch_to_amo error, size should be 8 or 4"
      )
      // back to back
      send_tlb_req()
    }.otherwise {
      state := sWaitFifo
    }
  }

  switch(state) {
    is(sIdle) {
      send_tlb_req()
    }
    is(sWaitTLBRsp) {
      io.tlb_resp.ready := true.B && !io.flush
      when(io.tlb_resp.fire && !io.flush) {
        // 1. flush is false, and tlb_resp is fire
        tlb_resp_buf := io.tlb_resp.bits
        // send request to StoreQueue in order to check addr conflict
        io.store_bypass.valid := true.B
        io.store_bypass.paddr := io.tlb_resp.bits.paddr
        when(check_privilege(io.tlb_resp.bits)) {
          dispatch_to_exception(io.tlb_resp.bits)
        }.elsewhen(io.tlb_resp.bits.req_type === TLBReqType.STORE) {
          dispatch_to_store(io.tlb_resp.bits)
        }.elsewhen(io.tlb_resp.bits.req_type === TLBReqType.LOAD) {
          dispatch_to_load(io.tlb_resp.bits)
        }.elsewhen(io.tlb_resp.bits.req_type === TLBReqType.AMO) {
          dispatch_to_amo(io.tlb_resp.bits)
        }.otherwise {
          assert(false.B, "sWaitTLBRsp error, should not reach here")
        }
      }.elsewhen(io.flush) {
        // 2. flush is true, cancel wait response
        state := sIdle
      }.otherwise {
        // 4. flush is false, and tlb_resp is not fire,continue to wait
        state := sWaitTLBRsp
      }
    }
    is(sWaitFifo) {
      when(!io.flush) {
        // send request to StoreQueue in order to check addr conflict
        io.store_bypass.valid := true.B
        io.store_bypass.paddr := tlb_resp_buf.paddr
        when(check_privilege(tlb_resp_buf)) {
          dispatch_to_exception(tlb_resp_buf)
        }.elsewhen(tlb_resp_buf.req_type === TLBReqType.STORE) {
          dispatch_to_store(tlb_resp_buf)
        }.elsewhen(tlb_resp_buf.req_type === TLBReqType.LOAD) {
          dispatch_to_load(tlb_resp_buf)
        }.elsewhen(io.tlb_resp.bits.req_type === TLBReqType.AMO) {
          dispatch_to_amo(tlb_resp_buf)
        }.otherwise {
          assert(false.B, "sWaitFifo error, should not reach here")
        }
      }.otherwise {
        state := sIdle
      }
    }
  }
}

object gen_agu_verilog extends App {
  GenVerilogHelper(new AGU())
}
