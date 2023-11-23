package leesum.axi4
import chisel3._
import chisel3.util.{Arbiter, Enum, is, log2Ceil, log2Up, switch}
import chiseltest.ChiselScalatestTester
import chiseltest.formal.{BoundedCheck, Formal, stable}
import leesum.{FormalUtils, GenVerilogHelper, ReqRespArbiter}
import org.scalatest.flatspec.AnyFlatSpec

class AxiReadArbiter extends Module {

  val io = IO(new Bundle {
    val in = Vec(2, new AXISlaveIO(32, 64))
    val out = new AXIMasterIO(32, 64)
  })

  io.in.foreach(_.ar.nodeq())
  io.in.foreach(_.r.noenq())
  io.in.foreach(_.aw.nodeq())
  io.in.foreach(_.w.nodeq())
  io.in.foreach(_.b.noenq())

  io.out.ar.noenq()
  io.out.r.nodeq()
  io.out.aw.noenq()
  io.out.w.noenq()
  io.out.b.nodeq()

  val axi_r_arb = Module(
    new ReqRespArbiter(2, new AXIAddressChannel(32), new AXIReadDataChannel(64))
  )

  axi_r_arb.io.flush := false.B
  axi_r_arb.io.req_vec <> io.in.map(_.ar)
  axi_r_arb.io.resp_vec <> io.in.map(_.r)
  io.out.ar <> axi_r_arb.io.req_arb
  io.out.r <> axi_r_arb.io.resp_arb

}

class AxiWriteArbiter extends Module {

  val io = IO(new Bundle {
    val in = Vec(2, new AXISlaveIO(32, 64))
    val out = new AXIMasterIO(32, 64)
  })

  io.in.foreach(_.ar.nodeq())
  io.in.foreach(_.r.noenq())
  io.in.foreach(_.aw.nodeq())
  io.in.foreach(_.w.nodeq())
  io.in.foreach(_.b.noenq())

  io.out.ar.noenq()
  io.out.r.nodeq()
  io.out.aw.noenq()
  io.out.w.noenq()
  io.out.b.nodeq()

  val axi_aw_b_arb = Module(
    new ReqRespArbiter(
      2,
      new AXIAddressChannel(32),
      new AXIWriteResponseChannel
    )
  )

  when(axi_aw_b_arb.io.sel_idx.valid) {
    io.out.w <> io.in(axi_aw_b_arb.io.sel_idx.bits).w
  }

  axi_aw_b_arb.io.flush := false.B
  axi_aw_b_arb.io.req_vec <> io.in.map(_.aw)
  axi_aw_b_arb.io.resp_vec <> io.in.map(_.b)

  io.out.aw <> axi_aw_b_arb.io.req_arb
  io.out.b <> axi_aw_b_arb.io.resp_arb
}

class AXIMux(
    in_nums: Int,
    addr_width: Int,
    data_width: Int,
    formal: Boolean = false
) extends Module {
  require(in_nums > 0, "in_nums must be positive")
  require(addr_width == 32 || addr_width == 64, "addr_width must be 32 or 64")
  require(data_width == 32 || data_width == 64, "data_width must be 32 or 64")
  val io = IO(new Bundle {
    val in = Vec(in_nums, new AXISlaveIO(addr_width, data_width))
    val out = new AXIMasterIO(32, 64)
  })

  io.in.foreach(_.ar.nodeq())
  io.in.foreach(_.r.noenq())
  io.in.foreach(_.aw.nodeq())
  io.in.foreach(_.w.nodeq())
  io.in.foreach(_.b.noenq())

  io.out.ar.noenq()
  io.out.r.nodeq()
  io.out.aw.noenq()
  io.out.w.noenq()
  io.out.b.nodeq()

  if (in_nums == 1) {
    io.out <> io.in.head
  } else {

    // ------------------
    //  AXI read Mux
    // ------------------
    val axi_ar_r_arb = Module(
      new ReqRespArbiter(
        in_nums,
        new AXIAddressChannel(addr_width),
        new AXIReadDataChannel(data_width),
        formal
      )
    )

    axi_ar_r_arb.io.flush := false.B
    axi_ar_r_arb.io.req_vec <> io.in.map(_.ar)
    axi_ar_r_arb.io.resp_vec <> io.in.map(_.r)
    io.out.ar <> axi_ar_r_arb.io.req_arb
    io.out.r <> axi_ar_r_arb.io.resp_arb

    // ------------------
    //  AXI write Mux
    // ------------------
    val axi_aw_b_arb = Module(
      new ReqRespArbiter(
        in_nums,
        new AXIAddressChannel(addr_width),
        new AXIWriteResponseChannel,
        formal
      )
    )

    when(axi_aw_b_arb.io.sel_idx.valid) {
      io.out.w <> io.in(axi_aw_b_arb.io.sel_idx.bits).w
    }

    axi_aw_b_arb.io.flush := false.B
    axi_aw_b_arb.io.req_vec <> io.in.map(_.aw)
    axi_aw_b_arb.io.resp_vec <> io.in.map(_.b)

    io.out.aw <> axi_aw_b_arb.io.req_arb
    io.out.b <> axi_aw_b_arb.io.resp_arb
  }
}

class AXIDeMux(
    out_nums: Int,
    addr_width: Int,
    data_width: Int,
    formal: Boolean = false
) extends Module {
  require(out_nums > 1, "in_nums must be positive")
  require(addr_width == 32 || addr_width == 64, "addr_width must be 32 or 64")
  require(data_width == 32 || data_width == 64, "data_width must be 32 or 64")
  val io = IO(new Bundle {
    val in = new AXISlaveIO(addr_width, data_width)
    val r_sel = Input(UInt(log2Ceil(out_nums).W))
    val w_sel = Input(UInt(log2Ceil(out_nums).W))
    val out = Vec(out_nums, new AXIMasterIO(32, 64))
  })

  io.in.ar.nodeq()
  io.in.r.noenq()
  io.in.aw.nodeq()
  io.in.w.nodeq()
  io.in.b.noenq()

  io.out.foreach(_.ar.noenq())
  io.out.foreach(_.r.nodeq())
  io.out.foreach(_.aw.noenq())
  io.out.foreach(_.w.noenq())
  io.out.foreach(_.b.nodeq())

  // ------------------
  //  AXI read DeMux
  // ------------------
  val r_sel_buf = RegInit(0.U(log2Ceil(out_nums).W))

  val sRIdle :: sWaitRResp :: Nil = Enum(2)
  val r_state = RegInit(sRIdle)

  switch(r_state) {
    is(sRIdle) {
      io.in.ar <> io.out(io.r_sel).ar
      when(io.in.ar.fire) {
        r_sel_buf := io.r_sel
        r_state := sWaitRResp
      }
    }
    is(sWaitRResp) {
      io.in.r <> io.out(r_sel_buf).r
      when(io.in.r.fire && io.in.r.bits.last) {
        r_state := sRIdle
      }
    }
  }

  // ------------------
  //  AXI write DeMux
  // ------------------
  val w_sel_buf = RegInit(0.U(log2Ceil(out_nums).W))
  val sAWIdle :: sWSend :: sBWait :: Nil = Enum(3)
  val w_state = RegInit(sAWIdle)

  switch(w_state) {
    is(sAWIdle) {
      io.in.aw <> io.out(io.w_sel).aw
      when(io.in.aw.fire) {
        w_sel_buf := io.w_sel
        w_state := sWSend
      }
    }
    is(sWSend) {
      io.in.w <> io.out(w_sel_buf).w
      when(io.in.w.fire) {
        w_state := sBWait
      }
    }
    is(sBWait) {
      io.in.b <> io.out(w_sel_buf).b
      when(io.in.b.fire) {
        w_state := sAWIdle
      }
    }
  }

  // ---------------------
  // formal verification
  // ---------------------

  if (formal) {
    when(FormalUtils.StreamShouldStable(io.in.ar)) {
      assume(io.in.ar.valid)
      assume(stable(io.in.ar.bits))
      assume(io.r_sel < out_nums.U)
      assume(stable(io.r_sel))
    }
    when(FormalUtils.StreamShouldStable(io.in.r)) {
      assert(io.in.r.valid)
      assert(stable(io.in.r.bits))
    }
    when(FormalUtils.StreamShouldStable(io.in.aw)) {
      assume(io.in.aw.valid)
      assume(stable(io.in.aw.bits))
      assume(io.w_sel < out_nums.U)
      assume(stable(io.w_sel))
    }
    when(FormalUtils.StreamShouldStable(io.in.w)) {
      assume(io.in.w.valid)
      assume(stable(io.in.w.bits))
    }
    when(FormalUtils.StreamShouldStable(io.in.b)) {
      assert(io.in.b.valid)
      assert(stable(io.in.b.bits))
    }

    for (i <- 0 until out_nums) {
      when(FormalUtils.StreamShouldStable(io.out(i).ar)) {
        assert(io.out(i).ar.valid)
        assert(stable(io.out(i).ar.bits))
      }
      when(FormalUtils.StreamShouldStable(io.out(i).r)) {
        assume(io.out(i).r.valid)
        assume(stable(io.out(i).r.bits))
      }
      when(FormalUtils.StreamShouldStable(io.out(i).aw)) {
        assert(io.out(i).aw.valid)
        assert(stable(io.out(i).aw.bits))
      }
      when(FormalUtils.StreamShouldStable(io.out(i).w)) {
        assert(io.out(i).w.valid)
        assert(stable(io.out(i).w.bits))
      }
      when(FormalUtils.StreamShouldStable(io.out(i).b)) {
        assume(io.out(i).b.valid)
        assume(stable(io.out(i).b.bits))
      }
    }
  }

}

class AXIFormal extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "AXIMux" should "pass with assumption" in {
    verify(
      new AXIMux(8, 32, 32, formal = true),
      Seq(BoundedCheck(10))
    )
  }
  "AXIDeMux" should "pass with assumption" in {
    verify(
      new AXIDeMux(8, 32, 32, formal = true),
      Seq(BoundedCheck(10))
    )
  }
}

object genAxiArbiter extends App {
  GenVerilogHelper(new AXIMux(2, 32, 32))
}
