package leesum.ICache

import chisel3._
import chisel3.util.{
  Cat,
  Counter,
  Decoupled,
  Enum,
  PopCount,
  Queue,
  is,
  log2Ceil,
  switch
}
import chiseltest.ChiselScalatestTester
import chiseltest.formal.{BoundedCheck, CVC4EngineAnnotation, Formal, stable}
import leesum.Cache._
import leesum.Utils.{LFSRRand, OverrideWithMask}
import leesum._
import leesum.axi4.{AXIDef, AXIMasterIO, AXIMux}
import leesum.moniter.PerfMonitorCounter
import org.scalatest.flatspec.AnyFlatSpec

class DCacheTop extends Module {

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new DCacheReq))
    val resp = Decoupled(new DCacheResp)
    val flush = Input(Bool())

    // fencei
    val fencei = Input(Bool()) // when fencei is true, flush must be true
    val fencei_ack = Output(Bool())

    // perf monitor
    val perf_dcache = Output(new PerfMonitorCounter)

    // memory port
    val mem_master = new AXIMasterIO(32, 64)
  })

  val dcache = Module(new DCacheTopIn)
  io.req <> dcache.io.req
  io.resp <> dcache.io.resp
  io.flush <> dcache.io.flush
  io.fencei_ack <> dcache.io.fencei_ack
  io.fencei <> dcache.io.fencei
  io.perf_dcache <> dcache.io.perf_dcache

  val dcache_axi_mux = Module(new AXIMux(3, 32, 64))
  dcache_axi_mux.io.out <> io.mem_master

  dcache_axi_mux.io.in(0) <> dcache.io.mem_master
  dcache_axi_mux.io.in(1) <> dcache.io.write_back_master
  dcache_axi_mux.io.in(2) <> dcache.io.mmio_master
}

class DCacheTopIn(formal: Boolean = false) extends Module {

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new DCacheReq))
    val resp = Decoupled(new DCacheResp)
    val flush = Input(Bool())

    // fencei
    val fencei = Input(Bool()) // when fencei is true, flush must be true
    val fencei_ack = Output(Bool())
    // perf monitor
    val perf_dcache = Output(new PerfMonitorCounter)

    // memory port
    val mem_master = new AXIMasterIO(32, 64)
    val write_back_master = new AXIMasterIO(32, 64)
    val mmio_master = new AXIMasterIO(32, 64)
  })

  val dcache_way = 4
  val dcache_way_bits = log2Ceil(dcache_way)

  io.mem_master.clear()
  io.write_back_master.clear()
  io.mmio_master.clear()
  io.req.nodeq()
  io.resp.noenq()
  io.fencei_ack := false.B

  val sIdle :: sLoadLookup :: sStoreLookup :: sMiss :: sDirtyCheck :: sRefillSendAR :: sRefillWaitR :: sSendDcacheResp :: sMMIOReq :: sMMIOResp :: sFlushReq :: sFLushing :: sFlushACK :: Nil =
    Enum(13)
  val state = RegInit(sIdle)
  val dcache_req_buf = RegInit(0.U.asTypeOf(new DCacheReq))

  val dcache_perf = RegInit(0.U.asTypeOf(new PerfMonitorCounter))

  io.perf_dcache := dcache_perf

  val d_buf_lookup = new LookupField(dcache_req_buf.paddr(38, 0))
  val d_lookup = new LookupField(io.req.bits.paddr(38, 0))

  val dcache_n_way = Module(new DCacheNWay(dcache_way))

  dcache_n_way.io.req_valid := false.B
  dcache_n_way.io.req_type := DontCare
  dcache_n_way.io.req_addr := DontCare
  dcache_n_way.io.req_way := DontCare
  dcache_n_way.io.write_data := DontCare
  dcache_n_way.io.write_mask := DontCare
  dcache_n_way.io.clear_en := false.B

  val dcache_hit = dcache_n_way.io.lookup_hit_way.valid
  val dcache_hit_way = dcache_n_way.io.lookup_hit_way.bits
  val dcache_hit_rdata = dcache_n_way.io.lookup_hit_data

  val random_way = LFSRRand(dcache_way)

  val writeback_fifo_in = Wire(Decoupled(new CacheWBbundle))
  val writeback_fifo_out = Queue(writeback_fifo_in, 1)
  writeback_fifo_in.noenq()

  val mmio_fsm = Module(new CacheMMIOFSM)
  val writeback_fsm = Module(new CacheWriteBackFSM)

  writeback_fsm.io.wb_in <> writeback_fifo_out
  writeback_fsm.io.axi_master <> io.write_back_master

  mmio_fsm.io.load_req.noenq()
  mmio_fsm.io.load_resp.nodeq()
  mmio_fsm.io.store_req.noenq()
  mmio_fsm.io.store_resp.nodeq()
  mmio_fsm.io.axi_mem <> io.mmio_master

  val select_refill_way = RegInit(0.U(log2Ceil(dcache_way).W))
  val rlen_buf = RegInit(0.U(8.W))
  val refill_data_buf = RegInit(0.U.asTypeOf(Vec(2, UInt(64.W))))
  val dcache_resp_buf = RegInit(0.U.asTypeOf(new DCacheResp))

  val fencei_way = Counter(4)
  val fencei_idx = Counter(64)

  require(fencei_idx.value.getWidth == 6)
  require(fencei_way.value.getWidth == 2)

  def rev_dcache_store_req() = {
    io.req.ready := true.B && io.req.bits.is_store && !io.flush
    when(io.req.fire) {
      dcache_req_buf := io.req.bits
      // send store lookup request
      dcache_n_way.io.req_valid := true.B
      dcache_n_way.io.req_type := DCacheReqType.store_lookup
      dcache_n_way.io.req_addr := io.req.bits.paddr
      state := sStoreLookup
    }.otherwise {
      state := sIdle
    }
  }

  def rev_dcache_req() = {
    io.req.ready := true.B && !io.flush && !io.fencei
    when(io.req.fire) {
      dcache_req_buf := io.req.bits

      when(io.req.bits.is_mmio) {
        state := sMMIOReq
      }.elsewhen(io.req.bits.is_store) {
        // send load lookup request
        dcache_n_way.io.req_valid := true.B
        dcache_n_way.io.req_type := DCacheReqType.store_lookup
        dcache_n_way.io.req_addr := io.req.bits.paddr

        state := sStoreLookup
      }.otherwise {
        // send store lookup request
        dcache_n_way.io.req_valid := true.B
        dcache_n_way.io.req_type := DCacheReqType.load_lookup
        dcache_n_way.io.req_addr := io.req.bits.paddr
        state := sLoadLookup
      }
    }.elsewhen(io.fencei) {
      state := sFlushReq
      fencei_way.reset()
      fencei_idx.reset()
    }.otherwise {
      state := sIdle
    }
  }

  def send_dcache_resp(resp: DCacheResp, is_store: Boolean) = {
    io.resp.valid := true.B
    io.resp.bits := resp
    when(io.resp.fire) {
      // back to back

      if (is_store) {

        // TODO: BUG!
//        rev_dcache_store_req()
        state := sIdle
      } else {
        state := sIdle
        rev_dcache_req()
      }
    }.otherwise {
      // can not send response at this cycle
      dcache_resp_buf := resp
      state := sSendDcacheResp
    }
  }

  switch(state) {
    is(sIdle) { rev_dcache_req() }
    is(sLoadLookup) {
      assert(dcache_req_buf.is_load, "should be load")

      when(dcache_hit) {
        dcache_perf.inc_hit(1.U)
        // load send response
        val load_resp = Wire(new DCacheResp)
        load_resp.rdata := dcache_hit_rdata.asTypeOf(Vec(2, UInt(64.W)))(
          d_buf_lookup.aligned_offset
        )
        load_resp.id := dcache_req_buf.id
        load_resp.exception := DontCare
        // TODO: RAM IN TO RAM OUT
        send_dcache_resp(load_resp, false)
      }.otherwise {
        dcache_perf.inc_miss(1.U)
        // miss hit
        state := sMiss
      }
    }
    is(sStoreLookup) {
      assert(dcache_req_buf.is_store, "should be store")

      when(dcache_hit) {
        dcache_perf.inc_hit(1.U)
        // store send response
        val store_resp = Wire(new DCacheResp)
        store_resp.rdata := DontCare
        store_resp.id := dcache_req_buf.id
        store_resp.exception := DontCare

        // write dcache
        val d_wdata = Mux(
          d_buf_lookup.aligned_offset,
          Cat(dcache_req_buf.wdata, 0.U(64.W)),
          Cat(0.U(64.W), dcache_req_buf.wdata)
        )
        val d_wmask = Mux(
          d_buf_lookup.aligned_offset,
          Cat(dcache_req_buf.wstrb, 0.U(8.W)),
          Cat(0.U(8.W), dcache_req_buf.wstrb)
        )
        require(d_wdata.getWidth == 128)
        require(d_wmask.getWidth == 16)

        // TODO: use write buffer to cut RAM TO RAM
        dcache_n_way.io.req_valid := true.B
        dcache_n_way.io.req_type := DCacheReqType.write
        dcache_n_way.io.req_addr := dcache_req_buf.paddr
        dcache_n_way.io.write_data := d_wdata
        dcache_n_way.io.write_mask := d_wmask
        dcache_n_way.io.req_way := dcache_hit_way

        send_dcache_resp(store_resp, true)

      }.otherwise {
        dcache_perf.inc_miss(1.U)
        state := sMiss
      }
    }
    is(sMiss) {
      when(writeback_fifo_in.ready) {
        select_refill_way := random_way

        // choose the random way to refill
        // 1. first write back the dirty data
        dcache_n_way.io.req_valid := true.B
        dcache_n_way.io.req_type := DCacheReqType.read
        dcache_n_way.io.req_addr := dcache_req_buf.paddr
        dcache_n_way.io.req_way := random_way

        state := sDirtyCheck
      }
    }
    is(sDirtyCheck) {
      val is_dirty =
        dcache_n_way.io.read_data.dirty && dcache_n_way.io.read_data.valid
      when(is_dirty) {
        writeback_fifo_in.valid := true.B
        writeback_fifo_in.bits.tag := dcache_n_way.io.read_data.tag // TODO: BUG!, use the old tag!!!
        writeback_fifo_in.bits.idx := d_buf_lookup.index
        writeback_fifo_in.bits.cacheline := dcache_n_way.io.read_data.data
        assert(writeback_fifo_in.fire, "writeback_fifo_in should be fire")
      }
      state := sRefillSendAR
    }
    is(sRefillSendAR) {
      io.mem_master.ar.valid := true.B
      io.mem_master.ar.bits.addr := Cat(
        dcache_req_buf.paddr(63, 4),
        0.U(4.W)
      ) // aligned to 16 bytes
      io.mem_master.ar.bits.size := DcacheConst.SIZE8
      io.mem_master.ar.bits.burst := DcacheConst.BURST_INCR
      io.mem_master.ar.bits.len := 1.U
      io.mem_master.ar.bits.id := 0.U // TODO: not implemented now
      rlen_buf := 0.U

      when(io.mem_master.ar.fire) {
        state := sRefillWaitR
      }
    }
    is(sRefillWaitR) {
      io.mem_master.r.ready := true.B
      when(io.mem_master.r.fire) {
        // no flush
        refill_data_buf(rlen_buf) := io.mem_master.r.bits.data
        rlen_buf := rlen_buf + 1.U
        when(io.mem_master.r.bits.last) {
          state := sSendDcacheResp
          // we can get the all refill data the same cycle
          val refill_data_tmp = VecInit(
            refill_data_buf
              .dropRight(1)
              .appended(io.mem_master.r.bits.data)
          )

          dontTouch(refill_data_tmp)
          require(refill_data_tmp.length == refill_data_buf.length)
          // TODO: refill dcache , need compose data when write
          // write dcache
          val d_wdata = Mux(
            d_buf_lookup.aligned_offset,
            Cat(dcache_req_buf.wdata, 0.U(64.W)),
            Cat(0.U(64.W), dcache_req_buf.wdata)
          ).asTypeOf(Vec(16, UInt(8.W)))
          val d_wmask = Mux(
            d_buf_lookup.aligned_offset,
            Cat(dcache_req_buf.wstrb, 0.U(8.W)),
            Cat(0.U(8.W), dcache_req_buf.wstrb)
          )

          dcache_n_way.io.req_valid := true.B
          dcache_n_way.io.req_type := DCacheReqType.refill
          dcache_n_way.io.req_addr := dcache_req_buf.paddr
          dcache_n_way.io.req_way := select_refill_way
          dcache_n_way.io.write_data := OverrideWithMask(
            refill_data_tmp.asTypeOf(Vec(16, UInt(8.W))),
            Mux(dcache_req_buf.is_store, d_wmask, 0.U(16.W)),
            d_wdata
          ).asUInt
          dcache_n_way.io.write_mask := GenMaskOne(16, 16)

          // select the right data
          dcache_resp_buf.rdata := refill_data_tmp(d_buf_lookup.aligned_offset)
        }
      }
    }
    is(sSendDcacheResp) {
      send_dcache_resp(dcache_resp_buf, false)
    }
    is(sMMIOReq) {
      assert(dcache_req_buf.is_mmio, "should be mmio")

      mmio_fsm.io.load_req.valid := dcache_req_buf.is_load
      mmio_fsm.io.load_req.bits.paddr := dcache_req_buf.paddr
      mmio_fsm.io.load_req.bits.size := dcache_req_buf.size
      mmio_fsm.io.load_req.bits.is_mmio := true.B

      mmio_fsm.io.store_req.valid := dcache_req_buf.is_store
      mmio_fsm.io.store_req.bits.paddr := dcache_req_buf.paddr
      mmio_fsm.io.store_req.bits.size := dcache_req_buf.size
      mmio_fsm.io.store_req.bits.wdata := dcache_req_buf.wdata
      mmio_fsm.io.store_req.bits.wstrb := dcache_req_buf.wstrb
      mmio_fsm.io.store_req.bits.is_mmio := true.B

      assert(
        PopCount(
          Seq(mmio_fsm.io.load_req.valid, mmio_fsm.io.store_req.valid)
        ) <= 1.U,
        "mmio load and store should be mutually exclusive"
      )

      when(dcache_req_buf.is_load && mmio_fsm.io.load_req.fire) {
        state := sMMIOResp
      }.elsewhen(dcache_req_buf.is_store && mmio_fsm.io.store_req.fire) {
        state := sMMIOResp
      }
    }
    is(sMMIOResp) {
      assert(dcache_req_buf.is_mmio, "should be mmio")

      // handshake with mmio fsm
      mmio_fsm.io.load_resp.ready := io.resp.ready && dcache_req_buf.is_load
      mmio_fsm.io.store_resp.ready := io.resp.ready && dcache_req_buf.is_store
      io.resp.valid := Mux(
        dcache_req_buf.is_load,
        mmio_fsm.io.load_resp.valid,
        mmio_fsm.io.store_resp.valid
      )
      io.resp.bits.rdata := Mux(
        dcache_req_buf.is_load,
        mmio_fsm.io.load_resp.bits.data,
        DontCare
      )
      io.resp.bits.exception := Mux(
        dcache_req_buf.is_load,
        mmio_fsm.io.load_resp.bits.exception,
        mmio_fsm.io.store_resp.bits.exception
      )
      when(io.resp.fire) {
        // back to back
        rev_dcache_req()
      }
    }
    is(sFlushReq) {
      // read cache line
      assert(io.fencei, "should be fencei")
      // send dcache read req
      dcache_n_way.io.req_valid := true.B
      dcache_n_way.io.req_type := DCacheReqType.read
      dcache_n_way.io.req_addr := Cat(
        0.U(29.W), // tag not used
        fencei_idx.value, // idx
        0.U(4.W) // offset not used
      ) // aligned to 16 bytes
      dcache_n_way.io.req_way := fencei_way.value

      state := sFLushing
    }
    is(sFLushing) {
      assert(io.fencei, "should be fencei")

      val is_dirty =
        dcache_n_way.io.read_data.dirty && dcache_n_way.io.read_data.valid
      when(is_dirty) {
        writeback_fifo_in.valid := true.B
        writeback_fifo_in.bits.tag := dcache_n_way.io.read_data.tag
        writeback_fifo_in.bits.idx := fencei_idx.value
        writeback_fifo_in.bits.cacheline := dcache_n_way.io.read_data.data
        when(writeback_fifo_in.fire) {
          fencei_inc()
        }
      }.otherwise {
        fencei_inc()
      }
    }
    is(sFlushACK) {
      assert(io.fencei, "should be fencei")
      state := sIdle
      io.fencei_ack := true.B
    }
  }

  private def fencei_inc() = {
    state := sFlushReq
    fencei_way.inc()

    when(fencei_way.value === (fencei_way.range.end - 1).U) {
      fencei_idx.inc()
    }

    // the last cache line
    when(
      fencei_idx.value === (fencei_idx.range.end - 1).U && fencei_way.value === (fencei_way.range.end - 1).U
    ) {
      state := sFlushACK
    }
  }

  // --------------------------
  // formal
  // --------------------------

  when(io.fencei_ack) {
    assert(io.fencei)
  }

  when(io.mem_master.r.fire) {
    assume(io.mem_master.r.bits.id === 0.U)
    assume(io.mem_master.r.bits.resp === AXIDef.RESP_OKAY)
//    assume(io.mem_master.r.bits.last === true.B)
    assume(io.mem_master.r.bits.user === 0.U)
  }

  if (formal) {

    when(FormalUtils.StreamShouldStable(io.mem_master.ar)) {
      assert(io.mem_master.ar.valid)
      assert(stable(io.mem_master.ar.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.aw)) {
      assert(io.mem_master.aw.valid)
      assert(stable(io.mem_master.aw.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.w)) {
      assert(io.mem_master.w.valid)
      assert(stable(io.mem_master.w.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.r)) {
      assume(io.mem_master.r.valid)
      assume(stable(io.mem_master.r.bits))
    }
    when(FormalUtils.StreamShouldStable(io.mem_master.b)) {
      assume(io.mem_master.b.valid)
      assume(stable(io.mem_master.b.bits))
    }
  }
}

class DCacheFormal extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "DCache" should "pass with assumption" in {
    verify(
      new DCacheTopIn(formal = true),
      Seq(BoundedCheck(20), CVC4EngineAnnotation)
    )
  }
}

object gen_dcache_top_verilog extends App {
  GenVerilogHelper(new DCacheTop)
}
