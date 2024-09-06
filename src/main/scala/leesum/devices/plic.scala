package leesum.devices

import chisel3.{dontTouch, _}
import chisel3.util.Valid
import leesum.{GenVerilogHelper, RegMap}
import leesum.axi4.BasicMemoryIO
import leesum.InstType
import chisel3.util.Cat
import leesum.ZeroExt
import chisel3.util.MixedVecInit
import chisel3.util.log2Ceil
import leesum.Utils.HoldRegister

///* ref spike plic */
//const _PLIC_MAX_CONTEXTS: usize = 15872;
///*
// * The PLIC consists of memory-mapped control registers, with a memory map
// * as follows:
// *
// * base + 0x000000: Reserved (interrupt source 0 does not exist)
// * base + 0x000004: Interrupt source 1 priority
// * base + 0x000008: Interrupt source 2 priority
// * ...
// * base + 0x000FFC: Interrupt source 1023 priority
// * base + 0x001000: Pending 0
// * base + 0x001FFF: Pending
// * base + 0x002000: Enable bits for sources 0-31 on context 0
// * base + 0x002004: Enable bits for sources 32-63 on context 0
// * ...
// * base + 0x0020FC: Enable bits for sources 992-1023 on context 0
// * base + 0x002080: Enable bits for sources 0-31 on context 1
// * ...
// * base + 0x002100: Enable bits for sources 0-31 on context 2
// * ...
// * base + 0x1F1F80: Enable bits for sources 992-1023 on context 15871
// * base + 0x1F1F84: Reserved
// * ...		    (higher context IDs would fit here, but wouldn't fit
// *		     inside the per-context priority vector)
// * base + 0x1FFFFC: Reserved
// * base + 0x200000: Priority threshold for context 0
// * base + 0x200004: Claim/complete for context 0
// * base + 0x200008: Reserved
// * ...
// * base + 0x200FFC: Reserved
// * base + 0x201000: Priority threshold for context 1
// * base + 0x201004: Claim/complete for context 1
// * ...
// * base + 0xFFE000: Priority threshold for context 15871
// * base + 0xFFE004: Claim/complete for context 15871
// * base + 0xFFE008: Reserved
// * ...
// * base + 0xFFFFFC: Reserved
// */
//
///* Each interrupt source has a priority register associated with it. */
//const PRIORITY_BASE: u64 = 0;
//const _PRIORITY_PER_ID: u64 = 4;
//const PRIORITY_END: u64 = PENDING_BASE - 1;
//
///* Each interrupt source has a pending bit associated with it. */
//const PENDING_BASE: u64 = 0x1000;
//const PENDING_END: u64 = ENABLE_BASE - 1;
///*
// * Each hart context has a vector of interupt enable bits associated with it.
// * There's one bit for each interrupt source.
// */
//const ENABLE_BASE: u64 = 0x2000;
//const ENABLE_PER_HART: u64 = 0x80;
//const ENABLE_END: u64 = CONTEXT_BASE - 1;
///*
// * Each hart context has a set of control registers associated with it.  Right
// * now there's only two: a source priority threshold over which the hart will
// * take an interrupt, and a register to claim interrupts.
// */
//const CONTEXT_BASE: u64 = 0x200000;
//const CONTEXT_PER_HART: u64 = 0x1000;
//const CONTEXT_THRESHOLD: u64 = 0;
//const CONTEXT_CLAIM: u64 = 4;
object PlicConst {
  val priority_base = 0
  val priority_per_id = 4
  val pending_base = 0x1000
  val enable_base = 0x2000
  val enable_per_hart = 0x80
  val context_base = 0x200000
  val context_per_hart = 0x1000
  val context_threshold = 0
  val context_claim = 4
}

class PlicEnable(nums: Int) extends Bundle {
  val enable = Vec(nums, UInt(32.W))
  def idx(i: Int): Bool = {
    val enable_bits = enable.asUInt
    Mux(i.U === 0.U, false.B, enable_bits(i))
  }
}

class PlicPriority extends Bundle {
  val priority = UInt(32.W)
}

class PlicContext extends Bundle {
  val threshold = UInt(32.W)
  val claim = UInt(32.W)
}

object MaxPriorityIrqIdx {

  def apply(
      priority_regs: Vec[PlicPriority],
      pending_bits: Vec[Bool],
      enable_regs: PlicEnable,
      context_regs: PlicContext
  ): UInt = {
    require(priority_regs.length == pending_bits.length)
    val numIrqs = pending_bits.length
    val maxPriorityIrqIdx = Module(
      new MaxPriorityIrqIdx(numIrqs)
    )
    maxPriorityIrqIdx.io.priority_regs := priority_regs
    maxPriorityIrqIdx.io.pending_bits := pending_bits
    maxPriorityIrqIdx.io.enable_regs := enable_regs
    maxPriorityIrqIdx.io.context_regs := context_regs
    maxPriorityIrqIdx.io.max_irq_idx
  }
}

class MaxPriorityIrqIdx(
    numIrqs: Int
) extends Module {
  val io = IO(new Bundle {
    val priority_regs = Input(Vec(numIrqs, new PlicPriority))
    val pending_bits = Input(Vec(numIrqs, Bool()))
    val enable_regs = Input(new PlicEnable((numIrqs + 31) / 32))
    val context_regs = Input(new PlicContext)
    val max_irq_idx = Output(UInt(log2Ceil(numIrqs + 1).W))
  })

  require(io.priority_regs.length == io.pending_bits.length)
  require(numIrqs == io.priority_regs.length)

  // Priority 1 is the lowest
  // active priority, and priority 7 is the highest

  val irq_priority = Wire(Vec(io.priority_regs.length, UInt(3.W)))

  // calculate the priority of each interrupt, if 0 then it is disabled
  for (i <- 0 until io.priority_regs.length) {
    when(
      io.pending_bits(i) && io.enable_regs.idx(i) && io
        .priority_regs(i)
        .priority(2, 0) > io.context_regs.threshold(2, 0) // max 7 priority
    ) {
      irq_priority(i) := io.priority_regs(i).priority(2, 0)
    }.otherwise {
      irq_priority(i) := 0.U
    }
  }

  // find the highest priority interrupt (e.g. chisel-book p. 154)
  val scalaVector = irq_priority.zipWithIndex
    .map((x) => MixedVecInit(x._1, x._2.U(log2Ceil(numIrqs + 1).W)))
  val resFun2 = VecInit(scalaVector)
    .reduceTree((x, y) => Mux(x(0) >= y(0), x, y))

  val maxVal = resFun2(0)
  val maxIdx = resFun2(1)

  io.max_irq_idx := Mux(maxVal === 0.U, 0.U, maxIdx)
}

class plic(
    harts_map: Seq[Boolean], // true: smode enable,
    base_addr: Int = 0x0c00_0000,
    num_ints: Int
) extends Module {
  val io = IO(new Bundle {
    val mem = new BasicMemoryIO(32, 32)
    val irq_pendings = Input(Vec(num_ints, Bool()))
  })

  val context_width = log2Ceil(harts_map.size * 2 + 1)

  // claimed_bits is a vector of bits indicating which interrupts are claimed
  // 1. claimed_bit0 is always false
  // 2. claimed_bit will be set if the interrupt is claimed
  // 3. claimed_bit will be cleared if the interrupt is completed
  val claimed_bits = RegInit(VecInit(Seq.fill(io.irq_pendings.length)(false.B)))

  // Calculate the number of 32-bit registers needed
  val num_pending_regs = (num_ints + 31) / 32

  // pending_bits is a vector of bits indicating which interrupts are pending
  // 1. pending_bit0 is always false
  // 2. pending will not be set by source if it is already claimed
  val pending_bits = RegInit(VecInit(Seq.fill(io.irq_pendings.length)(false.B)))

  for (i <- 1 until num_ints) {
    when(!claimed_bits(i)) {
      pending_bits(i) := io.irq_pendings(i)
    }
  }

  // Initialize pending_regs as a vector of 32-bit UInt registers
  val pending_regs = Wire(Vec(num_pending_regs, UInt(32.W)))

  // Assign values to pending_regs
  for (i <- 0 until num_pending_regs) {
    val zero_ext_pendings =
      ZeroExt(pending_bits.asUInt, pending_bits.length, num_pending_regs * 32)
    pending_regs(i) := zero_ext_pendings((i + 1) * 32 - 1, i * 32)
  }

  val priority_regs = RegInit(
    // each interrupt source has a priority register associated with it.
    VecInit(Seq.fill(num_ints)(0.U.asTypeOf(new PlicPriority)))
  )

  val enable_regs = RegInit(
    // each context has a list of enable bits, one per interrupt source.
    VecInit(
      Seq.fill(harts_map.size * 2)(
        0.U.asTypeOf(new PlicEnable(num_pending_regs))
      )
    )
  )

  val context_regs = RegInit(
    VecInit(Seq.fill(harts_map.size * 2)(0.U.asTypeOf(new PlicContext)))
  )

  // first enable bit is always 0
  val first_enable_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(32.W)))
    read_result.valid := true.B
    read_result.bits := Cat(reg(31, 1), false.B)
    read_result
  }
  val first_enable_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(32.W)))
    write_result.valid := true.B
    write_result.bits := Cat(wdata(31, 1), false.B)
    reg := write_result.bits
    write_result
  }

  // --------------------------
  // claim function
  // --------------------------
  val claim_read = (addr: UInt, reg: UInt) => {
    val read_result = Wire(Valid(UInt(32.W)))
    val selected_context = addr(12 + context_width - 1, 12)

    read_result.valid := true.B
    read_result.bits := MaxPriorityIrqIdx(
      priority_regs,
      pending_bits,
      enable_regs(selected_context),
      context_regs(selected_context)
    )

    when(read_result.bits =/= 0.U) {
      assert(
        claimed_bits(
          read_result.bits(log2Ceil(io.irq_pendings.length) - 1, 0)
        ) === false.B,
        "claim error"
      )
      // set claimed bit
      claimed_bits(
        read_result.bits(log2Ceil(io.irq_pendings.length) - 1, 0)
      ) := true.B
    }
    read_result
  }

  // --------------------------
  // complete function
  // --------------------------

  val complete_write = (addr: UInt, reg: UInt, wdata: UInt) => {
    val write_result = Wire(Valid(UInt(reg.getWidth.W)))
    write_result.valid := true.B
    write_result.bits := wdata

    assert(wdata < num_ints.U, "complete error")
    claimed_bits(wdata(log2Ceil(io.irq_pendings.length) - 1, 0)) := false.B

    write_result
  }

  val plic_regs = new RegMap

  for (i <- 0 until num_ints) {
    // each interrupt source has a priority register associated with it.
    plic_regs.add_reg(
      base_addr + PlicConst.priority_base + i * PlicConst.priority_per_id,
      priority_regs(i).priority,
      plic_regs.normal_read,
      plic_regs.normal_write
    )
  }

  for (i <- 0 until num_pending_regs) {
    // each interrupt source has a pending bit associated with it.
    plic_regs.add_reg(
      base_addr + PlicConst.pending_base + i * 4,
      pending_regs(i),
      plic_regs.normal_read,
      plic_regs.empty_write
    )
  }

  for (i <- harts_map.indices) {
    val mmode_context_idx = i * 2
    val smode_context_idx = i * 2 + 1
    val smode_en = harts_map(i)
    // mmode
    for (j <- 0 until num_pending_regs) {
      // each context has a list of enable bits, one per interrupt source.
      plic_regs.add_reg(
        base_addr + PlicConst.enable_base + mmode_context_idx * PlicConst.enable_per_hart + j * 4,
        enable_regs(mmode_context_idx).enable(j),
        if (j == 0) { first_enable_read }
        else { plic_regs.normal_read },
        if (j == 0) { first_enable_write }
        else (plic_regs.normal_write)
      )
    }

    plic_regs.add_reg(
      // each context has a threshold register associated with it.
      base_addr + PlicConst.context_base + mmode_context_idx * PlicConst.context_per_hart + PlicConst.context_threshold,
      context_regs(mmode_context_idx).threshold,
      plic_regs.normal_read,
      plic_regs.normal_write
    )
    plic_regs.add_reg(
      // each context has a claim/complete register associated with it.
      base_addr + PlicConst.context_base + mmode_context_idx * PlicConst.context_per_hart + PlicConst.context_claim,
      context_regs(mmode_context_idx).claim,
      claim_read,
      complete_write
    )

    if (smode_en) {
      // smode

      for (j <- 0 until num_pending_regs) {
        // each context has a list of enable bits, one per interrupt source.
        plic_regs.add_reg(
          base_addr + PlicConst.enable_base + smode_context_idx * PlicConst.enable_per_hart + j * 4,
          enable_regs(smode_context_idx).enable(j),
          if (j == 0) { first_enable_read }
          else { plic_regs.normal_read },
          if (j == 0) { first_enable_write }
          else (plic_regs.normal_write)
        )
      }
      plic_regs.add_reg(
        base_addr + PlicConst.context_base + smode_context_idx * PlicConst.context_per_hart + PlicConst.context_threshold,
        context_regs(smode_context_idx).threshold,
        plic_regs.normal_read,
        plic_regs.normal_write
      )
      plic_regs.add_reg(
        base_addr + PlicConst.context_base + smode_context_idx * PlicConst.context_per_hart + PlicConst.context_claim,
        context_regs(smode_context_idx).claim,
        claim_read,
        complete_write
      )
    }
  }

  plic_regs.print_map()

  // --------------------------
  // read
  // --------------------------
  when(io.mem.i_rd) {
    when(!plic_regs.in_range(io.mem.i_raddr)) {
      printf("plic read out of range: %x\n", io.mem.i_raddr)
      assert(false.B)
    }
  }
  io.mem.o_rdata := HoldRegister(
    io.mem.i_rd,
    RegNext(plic_regs.read(io.mem.i_raddr).bits),
    1
  )
  // --------------------------
  // write
  // --------------------------
  when(io.mem.i_we) {
    plic_regs.write(io.mem.i_waddr, io.mem.i_wdata)
    when(!plic_regs.in_range(io.mem.i_waddr)) {
      printf("plic write out of range: %x\n", io.mem.i_waddr)
      assert(false.B)
    }
  }

  // --------------------------
  // assert
  // --------------------------
  assert(io.irq_pendings(0) === false.B, "plic irq0 should be false")
  assert(claimed_bits(0) === false.B, "claimed_bit0 should be false")
  assert(pending_bits(0) === false.B, "pending_bit0 should be false")

  enable_regs.foreach(x => {
    assert(x.enable(0)(0) === false.B, "enable_bit0 should be false")
  })

}

object gen_plic_verilog extends App {
  val harts_map = Seq(false, true)
  GenVerilogHelper(new plic(harts_map, 0x0c00_0000, 10))
}
