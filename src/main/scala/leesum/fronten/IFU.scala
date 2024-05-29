package leesum.fronten

import chisel3._
import chisel3.util.{BitPat, Decoupled, Enum, is, log2Ceil, switch}
import chiseltest.ChiselScalatestTester
import chiseltest.formal._
import leesum.Cache.{ICacheReq, ICacheResp}
import leesum.Utils.DecoderHelper
import leesum._
import leesum.bpu.{BPInfo, BPUTop, BTBEntry, PreDocode}
import leesum.moniter.PerfMonitorCounter
import org.scalatest.flatspec.AnyFlatSpec

class f2_f3_pipe_entry extends Bundle {
  val fetch_group = Vec(4, new INSTEntry)
  val fetch_offset = UInt(3.W) // offset pc in fetch_group
  val exception = new ExceptionEntry()
  val exception_pc = UInt(64.W)

  val bp_info = new BPInfo()

  def clear(): Unit = {
    fetch_group.foreach(_.clear())
    exception.clear()
    exception_pc := 0.U
    fetch_offset := 0.U
    bp_info.clear()
  }
}

class IFUTop(
    boot_pc: Long = 0x80000000L,
    rvc_en: Boolean = false,
    btb_way_count: Int = 2,
    formal: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val icache_req = Decoupled(new ICacheReq)
    val icache_resp = Flipped(Decoupled(new ICacheResp))
    val inst_fifo_pop = Vec(2, Decoupled(new INSTEntry))
    val flush = Input(Bool())
//    val f3_redirect_pc = Output(new RedirectPC())
    val commit_redirect_pc = Input(new RedirectPC())

    // bpu commit update port
    val cmt_update_pc = Input(UInt(39.W))
    val cmt_update_btb_en =
      Input(Bool()) // btb only update when branch predict fail
    val cmt_update_bim_en = Input(Bool()) // bim update for all branch
    val cmt_update_btb_data = Input(new BTBEntry())
    val cmt_update_bim_data = Input(UInt(2.W))
    val cmt_update_btb_way_sel = Input(UInt(log2Ceil(btb_way_count).W))

    // f1 bp performance monitor
    val perf_bp_f1 = Output(new PerfMonitorCounter)
  })

  val bp_f1_perf = RegInit(0.U.asTypeOf(new PerfMonitorCounter))

  io.perf_bp_f1 := bp_f1_perf

  val nextline_bp = Module(new BPUTop(btb_way_count, 1024, 2048))
  val f3_bpu_update_valid_next = RegInit(false.B)
  val f3_bpu_update_pc_next = RegInit(0.U(39.W))
  val f3_bpu_update_btb_data_next = RegInit(0.U.asTypeOf(new BTBEntry()))

  val f3_bpu_update_sel_way_next = RegInit(0.U(log2Ceil(btb_way_count).W))

  when(f3_bpu_update_valid_next) {
    f3_bpu_update_valid_next := false.B
  }

  // nextline_bp port init
  nextline_bp.io.lookup_en := false.B
  nextline_bp.io.lookup_pc := DontCare

  nextline_bp.io.cmt_update_btb_en := io.cmt_update_btb_en
  nextline_bp.io.cmt_update_bim_en := io.cmt_update_bim_en
  nextline_bp.io.cmt_update_bim_data := io.cmt_update_bim_data
  nextline_bp.io.cmt_update_btb_data := io.cmt_update_btb_data
  nextline_bp.io.cmt_update_pc := io.cmt_update_pc
  nextline_bp.io.cmt_update_btb_way_sel := io.cmt_update_btb_way_sel

  // f3 btb update port
  nextline_bp.io.f3_update_btb_en := f3_bpu_update_valid_next
  nextline_bp.io.f3_update_btb_pc := f3_bpu_update_pc_next
  nextline_bp.io.f3_update_btb_way_sel := f3_bpu_update_sel_way_next

//  // TODO: f3 btb update should refine!!!
//  nextline_bp.io.f3_update_btb_data.offset := 0.U
//  nextline_bp.io.f3_update_btb_data.is_rvc := false.B
//  nextline_bp.io.f3_update_btb_data.target_pc := "x80000000".asUInt
//  nextline_bp.io.f3_update_btb_data.bp_type := BpType.None

  nextline_bp.io.f3_update_btb_data := f3_bpu_update_btb_data_next

  nextline_bp.io.clear_en := false.B // TODO: fence.i

  val sIdle :: sWaitResp :: sWaitPipe :: sFlushResp :: Nil = Enum(4)

  val state = RegInit(sIdle)

  val pc_gen_stage = Module(new PCGenStage(boot_pc, rvc_en))

  val pc_f1_f2_buf = RegInit(0.U(64.W))

  val f2_f3_buf = Reg(new f2_f3_pipe_entry)

  val f3_flush_next = RegInit(false.B)
  when(f3_flush_next || io.flush) {
    f3_flush_next := false.B
  }
  val f3_redirect_next = RegInit(0.U.asTypeOf(new RedirectPC()))

  when(f3_redirect_next.valid) {
    f3_redirect_next.valid := false.B
  }

  pc_gen_stage.io.commit_redirect_pc := io.commit_redirect_pc
  pc_gen_stage.io.f3_redirect_pc := f3_redirect_next

  pc_gen_stage.io.f1_redirect_pc.valid := nextline_bp.io.bp_info.target_pc_taken && pc_gen_stage.io.pc.fire
  pc_gen_stage.io.f1_redirect_pc.target := nextline_bp.io.bp_info.target_pc

  pc_gen_stage.io.pc.nodeq()

  when(reset.asBool) {
    f2_f3_buf.clear()
  }

  val f2_f3_pipe = Module(new PipeLine(new f2_f3_pipe_entry))
  f2_f3_pipe.io.flush := io.flush || f3_flush_next
  f2_f3_pipe.io.in.valid := false.B
  f2_f3_pipe.io.in.bits.clear()
  f2_f3_pipe.io.out.nodeq()

  val inst_realign = Module(new InstReAlign(rvc_en))

  // when jump we should flush last half inst
  inst_realign.io.flush := io.flush || f3_flush_next || pc_gen_stage.io.f1_redirect_pc.valid
  inst_realign.io.input.noenq()
  inst_realign.io.output.nodeq()

  io.icache_req.noenq()
  io.icache_resp.nodeq()
//  io.f3_redirect_pc.valid := false.B
//  io.f3_redirect_pc.bits := 0.U

  def send_icache_req() = {
    // TODO: check it about flush
    io.icache_req.valid := pc_gen_stage.io.pc.valid && !io.flush && !f3_flush_next

    // 1. flush from commit
    // 2. flush from f3 branch prediction
    pc_gen_stage.io.pc.ready := io.icache_req.ready && !io.flush && !f3_flush_next
//    io.icache_req.bits.va := pc_gen_stage.io.pc.bits
    io.icache_req.bits.va := pc_gen_stage.io.npc
    when(io.icache_req.fire) {
      assert(!io.flush && !f3_flush_next)
      pc_f1_f2_buf := pc_gen_stage.io.npc
      // send nextline_bp request
      nextline_bp.io.lookup_en := true.B
      nextline_bp.io.lookup_pc := pc_gen_stage.io.npc
      state := sWaitResp
    }.otherwise {
      state := sIdle
    }
  }

  switch(state) {
    is(sIdle) {
      send_icache_req()
    }
    is(sWaitResp) {
      // TODO: branch prediction flush
      io.icache_resp.ready := true.B && !io.flush && !f3_flush_next
      when(io.icache_resp.fire) {
        assert(!io.flush && !f3_flush_next)
        inst_realign.io.input.valid := true.B
        inst_realign.io.output.ready := true.B
        inst_realign.io.input.bits.pc := pc_f1_f2_buf
        inst_realign.io.input.bits.payload := io.icache_resp.bits.payload

        // --------------------------
        // f2_f3_pipe
        // --------------------------
        f2_f3_pipe.io.in.valid := true.B
        f2_f3_pipe.io.in.bits.fetch_group := inst_realign.io.output.bits
        f2_f3_pipe.io.in.bits.fetch_offset := pc_f1_f2_buf(2, 0)
        f2_f3_pipe.io.in.bits.exception := io.icache_resp.bits.exception
        // nextline_bp info
        f2_f3_pipe.io.in.bits.bp_info := nextline_bp.io.bp_info

        // if last half is valid, put exception to the start of the instruction
        f2_f3_pipe.io.in.bits.exception_pc := Mux(
          inst_realign.io.last_half_valid,
          inst_realign.io.last_half_pc,
          pc_f1_f2_buf
        )

        when(f2_f3_pipe.io.in.fire) {
          send_icache_req()
        }.otherwise {
          // --------------------------
          // f2_f3_buf
          // --------------------------
          f2_f3_buf.exception := io.icache_resp.bits.exception

          f2_f3_buf.bp_info := nextline_bp.io.bp_info

          // if last half is valid, put exception to the start of the instruction
          f2_f3_buf.exception_pc := Mux(
            inst_realign.io.last_half_valid,
            inst_realign.io.last_half_pc,
            pc_f1_f2_buf
          )

          f2_f3_buf.fetch_group := inst_realign.io.output.bits
          f2_f3_buf.fetch_offset := pc_f1_f2_buf(2, 0)
          state := sWaitPipe
        }
      }.elsewhen(io.flush) {
        state := sIdle
      }.elsewhen(f3_flush_next) {
        state := sFlushResp
      }
    }
    is(sWaitPipe) {
      f2_f3_pipe.io.in.valid := true.B && !io.flush && !f3_flush_next
      f2_f3_pipe.io.in.bits := f2_f3_buf
      when(f2_f3_pipe.io.in.fire) {
        send_icache_req()
      }.elsewhen(io.flush || f3_flush_next) {
        state := sIdle
      }
    }
    is(sFlushResp) {
      // TODO: What happens if f3_flush_next is true?
      io.icache_resp.ready := true.B && !io.flush
      when(io.icache_resp.fire || io.flush) {
//        assert(!f3_flush_next, "f3_flush_next should be false")
        state := sIdle
      }
    }
  }

  // --------------------------
  // f3
  // --------------------------

  val inst_fifo = Module(new InstsFIFO)

  inst_fifo.io.flush := io.flush
  io.inst_fifo_pop <> inst_fifo.io.pop

  inst_fifo.io.push.valid := false.B
  inst_fifo.io.push.bits.foreach(x => {
    x.clear()
  })
  f2_f3_pipe.io.out.ready := inst_fifo.io.push.ready

  when(f2_f3_pipe.io.out.fire && !io.flush && !f3_flush_next) {
    when(f2_f3_pipe.io.out.bits.exception.valid) {
      // if exception happens, put exception to the first entry
      inst_fifo.io.push.bits.foreach(x => {
        x.clear() // TODO: just clear the valid bit
      })
      // override the first entry
      inst_fifo.io.push.bits(0).valid := true.B
      inst_fifo.io.push.bits(0).pc := f2_f3_pipe.io.out.bits.exception_pc
      inst_fifo.io.push.bits(0).exception := f2_f3_pipe.io.out.bits.exception
      inst_fifo.io.push.valid := true.B
    }.otherwise {

      val f3_bp_info = f2_f3_pipe.io.out.bits.bp_info

      assert(
        f3_bp_info.branch_offset(0) === false.B,
        "branch offset should be 2 aligned"
      )

      val f3_fetch_group_modify_bpu =
        WireDefault(f2_f3_pipe.io.out.bits.fetch_group)

      dontTouch(f3_fetch_group_modify_bpu)

      // --------------------------------------
      // insert branch prediction info to inst slot
      // --------------------------------------
      for (i <- 0 until 4) {
        when(f3_bp_info.valid) {

          val inst_is_rvc = f2_f3_pipe.io.out.bits.fetch_group(i).rvc
          // Override branch prediction
          val inst_offset = f2_f3_pipe.io.out.bits.fetch_group(i).pc(2, 0)
          val inst_end = inst_offset +& Mux(inst_is_rvc, 2.U, 4.U)
          val bp_pc_offset = f3_bp_info.branch_offset

          if (i == 0) {
            // first inst, may be cross line jump inst
            when(
              inst_offset === 6.U && bp_pc_offset === 0.U && !inst_is_rvc
                || InRange(bp_pc_offset, inst_offset, inst_end)
            ) {

              f3_fetch_group_modify_bpu(i).bp.bp_type := f3_bp_info.branch_type
              f3_fetch_group_modify_bpu(i).bp.predict_pc := f3_bp_info.target_pc

              f3_fetch_group_modify_bpu(i).bp.bim_value := f3_bp_info.bim_value
              f3_fetch_group_modify_bpu(i).bp.sel_way := f3_bp_info.sel_way

            }
          } else {

            // other inst, not cross line jump inst
            when(InRange(bp_pc_offset, inst_offset, inst_end)) {
              f3_fetch_group_modify_bpu(i).bp.bp_type := f3_bp_info.branch_type
              f3_fetch_group_modify_bpu(i).bp.predict_pc := f3_bp_info.target_pc

              f3_fetch_group_modify_bpu(i).bp.bim_value := f3_bp_info.bim_value
              f3_fetch_group_modify_bpu(i).bp.sel_way := f3_bp_info.sel_way
            }
          }
        }
      }

      // --------------------------------------
      // bpu alias check
      // --------------------------------------
      val pre_decodes = Seq.fill(4)(Module(new PreDocode))
      val bpu_alias_exist = WireDefault(false.B)
      val bpu_alias_pc = WireDefault(0.U(64.W))
      val bpu_alias_real_type = WireDefault(BpType.None)
      val bpu_alias_sel_way = f3_bp_info.sel_way
      val bpu_alias_mask = WireDefault("b1111".U(4.W))
      val bpu_alias_crossline_no_match = WireDefault(false.B)

      dontTouch(bpu_alias_exist)
      dontTouch(bpu_alias_pc)
      dontTouch(bpu_alias_real_type)
      dontTouch(bpu_alias_sel_way)

      for (i <- 0 until 4) {
        pre_decodes(i).io.inst := f3_fetch_group_modify_bpu(i).inst
        pre_decodes(i).io.pc := f3_fetch_group_modify_bpu(i).pc
      }

      val both_jump = f3_fetch_group_modify_bpu
        .zip(pre_decodes)
        .map { case (inst, pre_decode) =>
          val pre_decode_is_jump =
            pre_decode.io.jump_type =/= BpType.None && inst.valid
          val bp_is_jump =
            inst.bp.bp_type =/= BpType.None && f3_bp_info.valid && f3_bp_info.target_pc_taken
          pre_decode_is_jump && bp_is_jump
        }

      val both_jump2UInt = VecInit(both_jump).asUInt

      val bp_mask_decode = DecoderHelper(
        both_jump2UInt,
        "b1111".U(4.W),
        Seq(
          BitPat("b???1") -> "b0001".U(4.W),
          BitPat("b??10") -> "b0011".U(4.W),
          BitPat("b?100") -> "b0111".U(4.W)
        )
      )

      dontTouch(bp_mask_decode)
      dontTouch(both_jump2UInt)

      f3_fetch_group_modify_bpu.zip(pre_decodes).zipWithIndex.foreach {
        // TODO: use Mux1H ?
        case ((inst, pre_decode), i) =>
          when(inst.valid) {
            val inst_is_br = pre_decode.io.jump_type === BpType.Branch
            val inst_is_jal = pre_decode.io.jump_type === BpType.Jal
            val inst_is_jalr = pre_decode.io.jump_type === BpType.Jalr
            val inst_not_jump = pre_decode.io.jump_type === BpType.None
            val inst_is_jump = !inst_not_jump

            val inst_offset = inst.pc(2, 0)
            val bp_pc_offset = f3_bp_info.branch_offset

            // TODO: call and return RAS not implemented
            val bp_is_jump = inst.bp.bp_type =/= BpType.None

            val bp_type_not_match = WireDefault(false.B)
            val bp_pos_not_match = WireDefault(false.B)

            val bp_alias_mask_by_idx = GenMaskOne(4, i)

            val is_crossline = if (i == 0) {
              // first inst, may be cross line jump inst
              WireDefault(
                inst_offset === 6.U && !inst.rvc
              )
            } else {
              WireDefault(false.B)
            }

            dontTouch(is_crossline)

            when(bp_is_jump && inst_is_jump) {
              bp_type_not_match := inst.bp.bp_type =/= pre_decode.io.jump_type
              if (i == 0) {
                // first inst, may be cross line jump inst
                when(is_crossline) {
                  bp_pos_not_match := bp_pc_offset =/= 0.U
//                  bpu_alias_crossline_no_match := bp_pc_offset =/= 0.U
                }.otherwise {
                  bp_pos_not_match := inst_offset =/= bp_pc_offset
//                  bpu_alias_crossline_no_match := inst_offset =/= bp_pc_offset
                }
              } else {
                bp_pos_not_match := inst_offset =/= bp_pc_offset
              }
            }

            dontTouch(bp_pos_not_match)
            dontTouch(bp_type_not_match)
            dontTouch(inst_is_jump)
            dontTouch(bp_is_jump)

            when(inst_not_jump && bp_is_jump) {
              // 1. inst is not jump, but bp is jump (alias)
              bpu_alias_exist := true.B
              bpu_alias_pc := inst.pc
              bpu_alias_mask := bp_alias_mask_by_idx
              bpu_alias_real_type := pre_decode.io.jump_type
              if (i == 0) {
                // first inst, may be cross line jump inst
                bpu_alias_crossline_no_match := is_crossline
              }

            }

            when(bp_type_not_match) {
              // 2. inst is jump, bp is jump, but type not match (alias)
              bpu_alias_exist := true.B
              bpu_alias_pc := inst.pc
              bpu_alias_mask := bp_alias_mask_by_idx
              bpu_alias_real_type := pre_decode.io.jump_type
              if (i == 0) {
                // first inst, may be cross line jump inst
                bpu_alias_crossline_no_match := is_crossline
              }
            }
            when(bp_pos_not_match) {
              // 3. inst is jump, bp is jump, but offset not match (alias)
              bpu_alias_exist := true.B
              bpu_alias_pc := inst.pc
              bpu_alias_mask := bp_alias_mask_by_idx
              bpu_alias_real_type := pre_decode.io.jump_type

              if (i == 0) {
                // first inst, may be cross line jump inst
                bpu_alias_crossline_no_match := is_crossline
              }
            }
          }
      }

      // --------------------------------------
      // assign output
      // --------------------------------------

      // inst fifo
      val f3_fetch_group_modify_bpu_and_valid =
        WireDefault(f3_fetch_group_modify_bpu)

      dontTouch(f3_fetch_group_modify_bpu_and_valid)
      for (i <- 0 until 4) {
        // override valid with bpu_alias_mask
        val masked_valid =
          f3_fetch_group_modify_bpu(i).valid && bpu_alias_mask(
            i
          ) && bp_mask_decode(i)
        f3_fetch_group_modify_bpu_and_valid(i).valid := masked_valid

        // override real bp type
        f3_fetch_group_modify_bpu_and_valid(i).bp.bp_type := pre_decodes(
          i
        ).io.jump_type
      }
      inst_fifo.io.push.valid := f3_fetch_group_modify_bpu_and_valid
        .map(_.valid)
        .reduce(_ || _)
      inst_fifo.io.push.bits := f3_fetch_group_modify_bpu_and_valid

      // f3 flush
      when(bpu_alias_exist) {
        assert(f3_bp_info.valid, "bpu_alias_exist should be true")

        bp_f1_perf.inc_miss(1.U)

        f3_flush_next := true.B
        f3_redirect_next.valid := true.B
        f3_redirect_next.target := bpu_alias_pc

        // update bpu (btb)
        f3_bpu_update_valid_next := true.B
        // we use upper pc to update bpu, when is cross line jump inst
        f3_bpu_update_pc_next := Mux(
          bpu_alias_crossline_no_match,
          bpu_alias_pc + 2.U,
          bpu_alias_pc
        )
        f3_bpu_update_sel_way_next := bpu_alias_sel_way
      }.elsewhen(f3_bp_info.valid && !bpu_alias_exist) {
        bp_f1_perf.inc_hit(1.U)
      }

    }
  }
  // --------------------------
  // formal
  // --------------------------

  when(RegNext(io.flush)) {
    assert(state === sIdle)
  }

  when(io.flush || f3_flush_next) {
    assume(io.icache_req.fire === false.B)
    assert(!pc_gen_stage.io.pc.ready)
  }

  when(io.inst_fifo_pop.map(_.fire).reduce(_ || _)) {
    assume(CheckOrder(VecInit(io.inst_fifo_pop.map(_.fire))))
  }

  if (formal) {
    val f_flush = io.flush && past(io.flush)

    when(FormalUtils.StreamShouldStable(io.icache_resp) && f_flush) {
      assume(io.icache_resp.valid)
      assume(stable(io.icache_resp.bits))
    }
  }

}

class IFUTopFormal extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "IFUTop" should "pass with assumption" in {
    verify(
      new IFUTop(formal = true),
      Seq(BoundedCheck(10), CVC4EngineAnnotation)
    )
  }
}

object gen_ifu_verilog extends App {
  GenVerilogHelper(new IFUTop)
}
