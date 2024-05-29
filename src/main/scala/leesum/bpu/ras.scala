package leesum.bpu

import chisel3._
import chisel3.util.{Counter, PopCount, isPow2, log2Ceil}
import leesum.GenVerilogHelper

class RAS(ras_nums: Int) extends Module {
  val io = IO(new Bundle {
    // push
    val push_cond = Input(Bool())
    val push_target = Input(UInt(64.W))
    // pop
    val pop_cond = Input(Bool())
    // refill
    val refill_en = Input(Bool())
    val refill_ras_stack = Input(Vec(ras_nums, UInt(64.W)))
    val refill_ras_occupied = Input(UInt((log2Ceil(ras_nums) + 1).W))
    val refill_top_ptr = Input(UInt(log2Ceil(ras_nums).W))
    // output
    val ras_top = Output(UInt(64.W))
    val ras_empty = Output(Bool())
    val ras_full = Output(Bool())
  })

  val ras_in = new RASIn(ras_nums)

  ras_in.push_pop_cond(io.push_cond, io.push_target, io.pop_cond)
  ras_in.refill(
    io.refill_en,
    io.refill_ras_stack,
    io.refill_ras_occupied,
    io.refill_top_ptr
  )

  io.ras_top := ras_in.ras_top
  io.ras_empty := ras_in.ras_empty
  io.ras_full := ras_in.ras_full
}

class RASIn(ras_nums: Int) {
  require(ras_nums > 0 && isPow2(ras_nums), "ras_nums should be power of 2")
  val ras_stack = RegInit(VecInit(Seq.fill(ras_nums)(0.U(64.W))))

  val top_ptr = RegInit(0.U(log2Ceil(ras_nums).W))
  val ras_occupied = RegInit(0.U((log2Ceil(ras_nums) + 1).W))
  val ras_empty = ras_occupied === 0.U
  val ras_full = ras_occupied === ras_nums.U

  def ras_top: UInt = {
    ras_stack(top_ptr)
  }

  def occupied_inc(): Unit = {
    when(!ras_full) {
      ras_occupied := ras_occupied + 1.U
    }
  }
  def occupied_dec(): Unit = {
    when(!ras_empty) {
      ras_occupied := ras_occupied - 1.U
    }
  }

  def refill(
      refill_en: Bool,
      new_ras_stack: Vec[UInt],
      new_ras_occupied: UInt,
      new_top_ptr: UInt
  ): Unit = {
    require(
      new_ras_stack.size == ras_nums,
      "new_ras_stack size should be equal to ras_nums"
    )
    require(
      new_ras_occupied.getWidth == ras_occupied.getWidth
    )
    require(new_top_ptr.getWidth == top_ptr.getWidth)

    when(refill_en) {
      ras_stack := new_ras_stack
      ras_occupied := new_ras_occupied
      top_ptr := new_top_ptr
    }

  }

  def push_pop_cond(
      push_cond: Bool,
      push_target: UInt,
      pop_cond: Bool
  ): Unit = {
    when(push_cond) {
      val next_top = top_ptr + 1.U
      ras_stack(next_top) := push_target
      occupied_inc()
      top_ptr := next_top
    }.elsewhen(pop_cond) {
      occupied_dec()
      top_ptr := top_ptr - 1.U
    }

    // ----------------------------
    // Assert
    // ----------------------------
    assert(
      PopCount(Seq(push_cond, pop_cond)) <= 1.U,
      "push_cond and pop_cond should not be true at the same time"
    )
    assert(
      !(pop_cond && ras_empty),
      "ras is empty, should not pop"
    )
  }
}

object gen_ras_verilog extends App {
  GenVerilogHelper(new RAS(8))
}

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.reflect.DataMirror
import chisel3.util._

class IndexableMem[T <: Data](
    entries: Int,
    gen: T,
    mem: Boolean,
    init: Option[Seq[T]]
) {
  require(!(init.nonEmpty && init.get.size != entries))
  val ram = Mem(entries, gen)
  val vec = Reg(Vec(entries, gen))
  val initializedVec = if (init.nonEmpty) RegInit(VecInit(init.get)) else null
  def apply(idx: UInt): T = {
    if (mem) ram(idx)
    else if (init.nonEmpty) initializedVec(idx)
    else vec(idx)
  }
}

object IndexableMem {
  def apply[T <: Data](
      entries: Int,
      gen: T,
      mem: Boolean = false,
      init: Option[Seq[T]] = None
  ): IndexableMem[T] = {
    new IndexableMem[T](entries, gen, mem, init)
  }
}

class MIMOQueueIO[T <: Data](gen: T, entries: Int, inCnt: Int, outCnt: Int)
    extends Bundle {
  val flush = Input(Bool())
  val enq = Vec(inCnt, Flipped(DecoupledIO(gen)))
  val deq = Vec(outCnt, DecoupledIO(gen))

}

class MIMOQueue[T <: Data](
    gen: T,
    val entries: Int,
    val inCnt: Int,
    val outCnt: Int,
    mem: Boolean = false,
    perf: Boolean = false,
    init: Option[Seq[T]] = None
) extends Module {

  require(isPow2(entries), "MIMOQueue: entries must be a power of 2!")
  require(!(init.nonEmpty && mem), "MIMOQueue: Mem can't be init!")
  require(!(init.nonEmpty && init.get.size != entries))

  def ptr_width = log2Up(entries)

  val io = IO(new MIMOQueueIO[T](gen, entries, inCnt, outCnt))

  val genType = {
    requireIsChiselType(gen)
    gen
  }

  val ram = IndexableMem(entries, genType, mem, init)

  val valids = if (perf) {
    RegInit(
      VecInit(
        (0 until entries).map(_ => if (init.nonEmpty) true.B else false.B)
      )
    )
  } else null

  val enqPtrInitVal = 0.U((ptr_width + 1).W)
  val deqPtrInitVal =
    (if (init.nonEmpty) 1 << ptr_width else 0).U((ptr_width + 1).W)

  val enq_ptr = RegInit(enqPtrInitVal)
  val deq_ptr = RegInit(deqPtrInitVal)

  def ptrToIdx(ptr: UInt): UInt = ptr.tail(1)
  def isFull(ptr1: UInt, ptr2: UInt): Bool =
    (ptr1.head(1) =/= ptr2.head(1)) && (ptr1.tail(1) === ptr2.tail(1))
  def isEmpty(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2

  def genPtrs(init: UInt, vec: Vec[DecoupledIO[T]]) = {
    if (perf) {
      vec.indices.map(i => {
        init + PopCount(vec.take(i).map(_.fire))
      })
    } else {
      val ptrs = vec.map(_ => Wire(UInt((ptr_width + 1).W)))
      for (i <- vec.indices) {
        ptrs(i) := { if (i == 0) init else ptrs(i - 1) + vec(i - 1).fire }
      }
      ptrs
    }
  }

  // dequeue
  val deq_ptrs = genPtrs(deq_ptr, io.deq)

  for ((deq, deq_ptr_wire) <- io.deq.zip(deq_ptrs)) {
    val deq_idx = ptrToIdx(deq_ptr_wire)
    deq.valid := {
      if (perf) valids(deq_idx)
      else !isEmpty(deq_ptr_wire, enq_ptr)
    }
    deq.bits := ram(deq_idx)
    if (perf) when(deq.fire) { valids(deq_idx) := false.B }
  }

  deq_ptr := deq_ptrs.last + io.deq(outCnt - 1).fire

  // enqueue
  val enq_ptrs = genPtrs(enq_ptr, io.enq)

  for ((enq, enq_ptr_wire) <- io.enq.zip(enq_ptrs)) {
    val enq_idx = ptrToIdx(enq_ptr_wire)
    enq.ready := {
      if (perf) !valids(enq_idx)
      else !isFull(enq_ptr_wire, deq_ptr)
    }
    when(enq.fire) {
      ram(enq_idx) := enq.bits
      if (perf) {
        valids(enq_idx) := true.B
      }
    }
  }

  enq_ptr := enq_ptrs.last + io.enq(inCnt - 1).fire

  when(io.flush) {
    deq_ptr := 0.U
    enq_ptr := 0.U
    if (perf) valids.foreach(_ := false.B)
  }

  // Debug(false){
  //   val cnt = RegInit((if(init.nonEmpty) entries else 0).U(32.W))
  //   val enqCnt = PopCount(io.enq.map(_.fire))
  //   val deqCnt = PopCount(io.deq.map(_.fire))
  //   cnt := cnt + enqCnt - deqCnt
  //   assert(cnt > deqCnt, "MIMOQueue underflow!")
  //   assert(cnt + enqCnt < entries.U(32.W), "MIMOQueue overflow!")
  //   printf(p"cnt: $cnt enqCnt:$enqCnt deqCnt:$deqCnt\n")
  // }

}

object gen_mimo_queue_verilog extends App {
  GenVerilogHelper(
    new MIMOQueue(UInt(32.W), 16, 4, 2, mem = true, perf = true)
  )
}
