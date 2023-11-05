package leesum

import chisel3._
import chisel3.util.{Cat, Decoupled, ListLookup, MuxLookup}

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

class InstsItem extends Bundle {
  val insts_vec = Output(Vec(4, new INSTEntry))

  def insts(idx: UInt): UInt = {
    insts_vec(idx).inst
  }

  def clear(): Unit = {
    for (i <- 0 until 4) {
      val tmp = Wire(new INSTEntry)
      tmp.clear()
      insts_vec(i) := tmp
    }
  }
}

/** This module convert a RspPacket(8 byte size) to InstsItem. The InstsItem may
  * contain 1 to 4 insts, and the insts may be 16 bits or 32 bits. This module
  * is combination logic,Return the InstsItem at the same cycle when input fire.
  */
class InstReAlign(rvc_en: Boolean = false) extends Module {
  val input = IO(Flipped(Decoupled(new RspPacket)))
  val output = IO(
    Decoupled(new InstsItem)
  )
  val flush = IO(Input(Bool()))

  val last_half_inst = RegInit(0.U(16.W));
  val last_half_valid = RegInit(false.B)

  val aligned_pc = Cat(input.bits.pc(63, 3), 0.U(3.W))

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

  val inst_mask_vec = inst_mask(input.bits.pc)

  def is_occupied(idx: Int): Bool = {
    val ret = idx match {
      case -1 => !last_half_valid
      case 0 =>
        val pre_inst_is_32bits =
          (!is_occupied(idx - 1)) & (!RISCVPkg.is_rvc(
            Cat(
              input.bits.inst_packet(idx)(15, 0),
              last_half_inst
            )
          ));
        val current_is_rvc = (is_occupied(idx - 1)) & RISCVPkg.is_rvc(
          input.bits.inst_packet(idx)
        );

        pre_inst_is_32bits || current_is_rvc || !inst_mask_vec(idx)
      case _ =>
        val pre_inst_is_32bits =
          (!is_occupied(idx - 1)) & (!RISCVPkg.is_rvc(
            input.bits.inst_packet(idx - 1)
          ))
        val current_is_rvc = is_occupied(idx - 1) & RISCVPkg.is_rvc(
          input.bits.inst_packet(idx)
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
  when(input.fire) {
    last_half_inst := input.bits.inst_packet(3)
    when(is_occupied(3)) {
      last_half_valid := false.B
    }.otherwise {
      last_half_valid := true.B
    }
  }

  when(flush) {
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

  output.bits.insts_vec(0) := realign_inst(
    aligned_pc,
    last_half_inst,
    input.bits.inst_packet(0),
    is_occupied(-1),
    is_occupied(0)
  )

  output.bits.insts_vec(1) := realign_inst(
    aligned_pc + 2.U,
    input.bits.inst_packet(0),
    input.bits.inst_packet(1),
    is_occupied(0),
    is_occupied(1)
  )

  output.bits.insts_vec(2) := realign_inst(
    aligned_pc + 4.U,
    input.bits.inst_packet(1),
    input.bits.inst_packet(2),
    is_occupied(1),
    is_occupied(2)
  )

  output.bits.insts_vec(3) := realign_inst(
    aligned_pc + 6.U,
    input.bits.inst_packet(2),
    input.bits.inst_packet(3),
    is_occupied(2),
    is_occupied(3)
  )

  // override invalid insts
  output.bits.insts_vec.zip(inst_mask_vec).foreach { case (inst, mask) =>
    when(!mask) {
      inst.valid := false.B
    }
  }

  input.ready := output.ready
  output.valid := input.valid
}

object gen_verilog extends App {

  GenVerilogHelper(new InstReAlign())
}
