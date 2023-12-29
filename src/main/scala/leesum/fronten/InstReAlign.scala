package leesum.fronten

import chisel3._
import chisel3.util.{Cat, Decoupled, MuxLookup}
import chiseltest.ChiselScalatestTester
import chiseltest.formal.{BoundedCheck, CVC4EngineAnnotation, Formal}
import leesum.Utils.VecCompressorNew
import leesum._
import org.scalatest.flatspec.AnyFlatSpec

// TODO: exception
class RspPacket extends Bundle {
  // config
  private val pc_width = 64
  private val payload_width = 64

  // inst_packet are aligned to 16 bits in memory
  private val inst_boundary = 16

  val max_inst_packet = payload_width / inst_boundary

  // port
  val pc = UInt(pc_width.W)
  val payload = UInt(64.W)

  /** A inst packet is 16 bits
    *
    * @param idx
    * @return
    */
  def inst_packet(idx: Int): UInt = {
    require(idx >= 0 && idx < max_inst_packet)
    val x = payload(16 * idx + 15, 16 * idx)
    require(x.getWidth == 16)
    x
  }

}

/** This module convert a RspPacket(8 byte size) to InstsItem. The InstsItem may
  * contain 1 to 4 insts, and the insts may be 16 bits or 32 bits. This module
  * is combination logic,Return the InstsItem at the same cycle when input fire.
  */
class InstReAlign(rvc_en: Boolean = false) extends Module {

  val io = IO(new Bundle {
    val input = Flipped(Decoupled(new RspPacket))
    val output = Decoupled(Vec(4, new INSTEntry))
    val last_half_valid = Output(Bool())
    val last_half_pc = Output(UInt(64.W))
    val flush = Input(Bool())
  })

  io.input.ready := io.output.ready
  io.output.valid := io.input.valid

  val unaligned_insts = io.input

  val last_half_inst = RegInit(0.U(16.W))
  val last_half_pc = RegInit(0.U(64.W))
  val last_half_valid = RegInit(false.B)

  val aligned_pc = Cat(unaligned_insts.bits.pc(63, 3), 0.U(3.W))

  io.last_half_valid := last_half_valid
  io.last_half_pc := last_half_pc

  def inst_mask(pc: UInt): Vec[Bool] = {
    val ret = MuxLookup(
      pc(2, 0),
      VecInit(Seq.fill(4)(false.B))
    )(
      Seq(
        0.U -> VecInit(Seq(true.B, true.B, true.B, true.B)),
        2.U -> VecInit(Seq(false.B, true.B, true.B, true.B)),
        4.U -> VecInit(Seq(false.B, false.B, true.B, true.B)),
        6.U -> VecInit(Seq(false.B, false.B, false.B, true.B))
      )
    )
    ret
  }

  val inst_mask_vec = inst_mask(unaligned_insts.bits.pc)

  def is_occupied(idx: Int): Bool = {
    val ret = idx match {
      case -1 => !last_half_valid
      case 0 =>
        val pre_inst_is_32bits =
          (!is_occupied(idx - 1)) & (!RISCVPkg.is_rvc(
            Cat(
              unaligned_insts.bits.inst_packet(idx)(15, 0),
              last_half_inst
            )
          ));
        val current_is_rvc = (is_occupied(idx - 1)) & RISCVPkg.is_rvc(
          unaligned_insts.bits.inst_packet(idx)
        );

        pre_inst_is_32bits || current_is_rvc || !inst_mask_vec(idx)
      case _ =>
        val pre_inst_is_32bits =
          (!is_occupied(idx - 1)) & (!RISCVPkg.is_rvc(
            unaligned_insts.bits.inst_packet(idx - 1)
          ))
        val current_is_rvc = is_occupied(idx - 1) & RISCVPkg.is_rvc(
          unaligned_insts.bits.inst_packet(idx)
        )

        pre_inst_is_32bits || current_is_rvc || !inst_mask_vec(idx)
    }

    if (idx < 0) {
      ret.suggestName(s"occupied_neg_${idx.abs}")
    } else {
      ret.suggestName(s"occupied_${idx}")
    }
    ret
  }

  // buffer last half inst
  when(unaligned_insts.fire) {
    last_half_inst := unaligned_insts.bits.inst_packet(3)
    when(is_occupied(3)) {
      last_half_valid := false.B
    }.otherwise {
      last_half_valid := true.B
      last_half_pc := aligned_pc + 6.U
    }
  }

  when(io.flush) {
    last_half_valid := false.B
  }

  def realign_inst(
      cur_pc: UInt,
      pre_packet: UInt,
      cur_packet: UInt,
      pre_is_occupied: Bool,
      cur_is_occupied: Bool
  ): INSTEntry = {
    require(cur_packet.getWidth == 16)
    require(pre_packet.getWidth == 16)
    val aligned_inst = Wire(new INSTEntry)
    aligned_inst.clear()

    when(cur_is_occupied) {
      when(pre_is_occupied) {
        // 16 bits inst
        aligned_inst.valid := true.B
        aligned_inst.inst := RVCExpander(cur_packet, rvc_en)
        aligned_inst.inst_c := cur_packet
        aligned_inst.rvc := true.B
        aligned_inst.pc := cur_pc
//        assert(RISCVPkg.is_rvc(cur_packet), "cur_packet must be rvc")

      }.otherwise {
        // 32 bits inst
        aligned_inst.valid := true.B
        aligned_inst.inst := Cat(cur_packet(15, 0), pre_packet(15, 0))
        aligned_inst.rvc := false.B
        aligned_inst.pc := cur_pc - 2.U

//        assert(
//          !RISCVPkg.is_rvc(Cat(cur_packet(15, 0), pre_packet(15, 0))),
//          "inst must be 32 bits"
//        )
      }
    }
    aligned_inst
  }

  def get_inst_pak(idx: Int): UInt = {
    val inst_pak = idx match {
      case -1 => last_half_inst
      case _  => unaligned_insts.bits.inst_packet(idx)
    }
    inst_pak
  }

  val realign_insts = Wire(Vec(4, new INSTEntry))

  for (i <- 0 until 4) {
    realign_insts(i) := realign_inst(
      aligned_pc + i.U * 2.U,
      get_inst_pak(i - 1),
      get_inst_pak(i),
      is_occupied(i - 1),
      is_occupied(i)
    )

    // override invalid insts
    when(!inst_mask_vec(i)) {
      realign_insts(i).valid := false.B
    }
  }

  val vec_compress = Module(new VecCompressorNew(new INSTEntry, 4))

  vec_compress.io.in.zipWithIndex.foreach {
    case (x, i) => {
      x.bits := realign_insts(i)
      x.valid := realign_insts(i).valid && io.input.valid
    }
  }

  // TODO: improve this
  for (i <- 0 until 4) {
    io.output.bits(i) := vec_compress.io.out(i).bits
    io.output.bits(i).valid := vec_compress.io.out(i).valid
  }

  // -----------------------------
  // formal
  // -----------------------------

  when(io.input.fire) {
    assume(io.input.bits.pc(0) === false.B)
  }

  when(io.output.fire) {
    assert(CheckOrder(VecInit(io.output.bits.map(_.valid))))
  }

}

class InstRealignFormal
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Formal {
  "InstRealign" should "pass with assumption" in {
    verify(
      new InstReAlign(),
      Seq(BoundedCheck(10), CVC4EngineAnnotation)
    )
  }
}

object gen_verilog extends App {

  GenVerilogHelper(new InstReAlign())
}
