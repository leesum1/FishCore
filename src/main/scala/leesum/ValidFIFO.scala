package leesum

import chisel3._
import chisel3.util.{Decoupled, Valid, isPow2, log2Ceil}

class IndexableMem[T <: Data](
    entries: Int,
    gen: T,
    mem: Boolean
) {
  require(isPow2(entries), "IndexableMem: entries must be a power of 2!")
  val ram = Mem(entries, gen)
  val vec = Reg(Vec(entries, gen))
  def apply(idx: UInt): T = {
    if (mem) ram(idx)
    else vec(idx)
  }
}
object IndexableMem {
  def apply[T <: Data](
      entries: Int,
      gen: T,
      mem: Boolean = false
  ): IndexableMem[T] = {
    new IndexableMem[T](entries, gen, mem)
  }
}

class MultiPortFIFOBase[T <: Data](
    gen: T,
    size: Int,
    num_push_ports: Int,
    num_pop_ports: Int,
    use_mem: Boolean = false,
    with_valid: Boolean = true
) {
  require(isPow2(size), "content must have power-of-2 number of entries")
  require(
    num_push_ports > 0,
    "MultiportFIFO must have non-zero number of push-ports"
  )
  require(
    num_pop_ports > 0,
    "MultiportFIFO must have non-zero number of pop-ports"
  )

  private val ram = IndexableMem(size, gen, use_mem)
  private val ram_valid = RegInit(VecInit(Seq.fill(size)(false.B)))

  val ptr_width = log2Ceil(size)
  val push_ptrs = RegInit(
    VecInit(Seq.tabulate(num_push_ports)(i => i.U(ptr_width.W)))
  )
  val pop_ptrs = RegInit(
    VecInit(Seq.tabulate(num_pop_ports)(i => i.U(ptr_width.W)))
  )

  val occupied_entries = RegInit(0.U((ptr_width + 1).W))
  val free_entries = RegInit(size.U((ptr_width + 1).W))

  val full = occupied_entries(ptr_width) === 1.U
  val empty = occupied_entries === 0.U

  def push_ptr_inc(size: UInt): Unit = {
    assert(size <= num_push_ports.U, "size must less than num_push_ports")
    push_ptrs.foreach({ ptr =>
      ptr := ptr + size
    })
  }
  def pop_ptr_inc(size: UInt): Unit = {
    assert(size <= num_pop_ports.U, "size must less than num_pop_ports")
    pop_ptrs.foreach({ ptr =>
      ptr := ptr + size
    })
  }
  def flush(flush_cond: Bool): Unit = {
    when(flush_cond) {
      occupied_entries := 0.U
      free_entries := size.U
      push_ptrs.zipWithIndex.foreach({ case (ptr, i) =>
        ptr := i.U
      })
      pop_ptrs.zipWithIndex.foreach({ case (ptr, i) =>
        ptr := i.U
      })
      if (with_valid) {
        ram_valid.foreach({ v =>
          v := false.B
        })
      }
    }
  }

  def peek(): Vec[Valid[T]] = {
    val last = Wire(Vec(num_pop_ports, Valid(gen)))
    for (i <- 0 until num_pop_ports) {
      val current_pop_ptr = pop_ptrs(i)
      last(i).bits := ram(current_pop_ptr)
      if (with_valid) {
        last(i).valid := ram_valid(current_pop_ptr)
      } else {
        last(i).valid := occupied_entries > i.U
      }
    }
    last
  }

  def read(addr: UInt): Valid[T] = {
    require(
      with_valid && !use_mem,
      "read only support with_valid and not use_mem"
    )

    assume(addr < size.U, "ptr must less than size")
    val data = ram(addr)
    val valid = ram_valid(addr)
    val res = Wire(new Valid(gen))
    res.bits := data
    res.valid := valid
    res
  }

  def content(addr: UInt) = {
    require(
      with_valid && !use_mem,
      "write only support with_valid and not use_mem"
    )
    ram(addr)
  }

  def content_valid(addr: UInt) = {
    require(
      with_valid && !use_mem,
      "write only support with_valid and not use_mem"
    )
    ram_valid(addr)
  }

  def push_pop_flush_cond(
      push_cond: Iterable[Bool],
      pop_cond: Iterable[Bool],
      flush_cond: Bool,
      entry: Iterable[T]
  ): Unit = {
    require(push_cond.size == num_push_ports)
    require(pop_cond.size == num_pop_ports)

    // -------------------
    // push
    // -------------------
    for (i <- 0 until num_push_ports) {
      when(push_cond.toSeq(i)) {
        val current_push_ptr = push_ptrs(i)
        ram(current_push_ptr) := entry.toSeq(i)
        if (with_valid) {
          ram_valid(current_push_ptr) := true.B
        }
      }
    }
    // -------------------
    // pop
    // -------------------
    for (i <- 0 until num_pop_ports) {
      when(pop_cond.toSeq(i)) {
        val current_pop_ptr = pop_ptrs(i)
        if (with_valid) {
          ram_valid(current_pop_ptr) := false.B
        }
      }
    }

    // -----------------------
    // update ptr
    // -----------------------
    val push_count = PopCountOrder(push_cond)
    val pop_count = PopCountOrder(pop_cond)
    push_ptr_inc(push_count)
    pop_ptr_inc(pop_count)
    occupied_entries := occupied_entries + push_count - pop_count
    free_entries := free_entries - push_count + pop_count

    // -----------------------
    // flash
    // -----------------------
    flush(flush_cond)
    // -----------------------
    // assert
    // -----------------------
    assert(CheckOrder(push_cond), "push_cond must be ordered")
    assert(CheckOrder(pop_cond), "pop_cond must be ordered")
    assert(push_count <= free_entries, "push_cond should not overflow")
    assert(
      pop_count <= occupied_entries,
      "pop_cond should not overflow"
    )

    when(RegNext(flush_cond)) {
      assert(occupied_entries === 0.U, "num_counter should be zero after flush")
      assert(free_entries === size.U, "free_entries should be size after flush")
      assert(
        pop_ptrs === VecInit(
          Seq.tabulate(num_pop_ports)(i => i.U(ptr_width.W))
        ),
        "pop_ptr_seq should be zero after flush"
      )
      assert(
        push_ptrs === VecInit(
          Seq.tabulate(num_push_ports)(i => i.U(ptr_width.W))
        ),
        "push_ptr_seq should be zero after flush"
      )
    }

    when(full) {
      assert(occupied_entries === size.U)
      assert(free_entries === 0.U)
    }
  }
}

class MultiPortFIFOUseMEM[T <: Data](
    gen: T,
    size: Int,
    num_push_ports: Int,
    num_pop_ports: Int
) {
  require(isPow2(size), "content must have power-of-2 number of entries")
  require(
    num_push_ports > 0,
    "MultiportFIFO must have non-zero number of push-ports"
  )
  require(
    num_pop_ports > 0,
    "MultiportFIFO must have non-zero number of pop-ports"
  )
  require(
    size % num_push_ports == 0,
    "size must be divisible by num_push_ports"
  )

  val lane_width = math.max(num_push_ports, num_pop_ports)
  val lane_depth = size / lane_width
  val lane_width_bits = log2Ceil(lane_width)
  val lane_seq = Seq.fill(lane_width)(Mem(lane_depth, gen))

  val ptr_width = log2Ceil(size)
  val push_ptrs = RegInit(
    VecInit(Seq.tabulate(lane_width)(i => i.U(ptr_width.W)))
  )
  val pop_ptrs = RegInit(
    VecInit(Seq.tabulate(lane_width)(i => i.U(ptr_width.W)))
  )

  dontTouch(push_ptrs)

  val occupied_entries = RegInit(0.U(log2Ceil(size + 1).W))
  val free_entries = RegInit(size.U(log2Ceil(size + 1).W))

  val full = occupied_entries(ptr_width) === 1.U
  val empty = occupied_entries === 0.U

  private def lane_idx(addr: UInt): UInt = {
    require(addr.getWidth == ptr_width, "addr width must equal ptr_width")
    addr(lane_width_bits - 1, 0)
  }
  private def lane_addr(addr: UInt): UInt = {
    require(addr.getWidth == ptr_width, "addr width must equal ptr_width")

    addr(ptr_width - 1, lane_width_bits)
  }

  def push_ptr_inc(size: UInt): Unit = {
    assert(size <= num_push_ports.U, "size must less than num_push_ports")
    push_ptrs.foreach({ ptr =>
      ptr := ptr + size
    })
  }
  def pop_ptr_inc(size: UInt): Unit = {
    assert(size <= num_pop_ports.U, "size must less than num_pop_ports")
    pop_ptrs.foreach({ ptr =>
      ptr := ptr + size
    })
  }
  def flush(flush_cond: Bool): Unit = {
    when(flush_cond) {
      occupied_entries := 0.U
      free_entries := size.U
      push_ptrs.zipWithIndex.foreach({ case (ptr, i) =>
        ptr := i.U
      })
      pop_ptrs.zipWithIndex.foreach({ case (ptr, i) =>
        ptr := i.U
      })
    }
  }

  def peek(): Vec[Valid[T]] = {
    val last = Wire(Vec(lane_width, Valid(gen)))

    val shifted_pop_ptrs =
      BarrelShifter.rightRotate(pop_ptrs, lane_idx(pop_ptrs(0)))

    val lane_rdata = lane_seq
      .zip(shifted_pop_ptrs)
      .map({ case (lane, ptr) =>
        lane(lane_addr(ptr))
      })
    val shifted_rdata =
      BarrelShifter.leftRotate(VecInit(lane_rdata), lane_idx(pop_ptrs(0)))

    for (i <- 0 until lane_width) {
      last(i).bits := shifted_rdata(i)
      last(i).valid := occupied_entries > i.U
    }
    last
  }

  def push_pop_flush_cond(
      push_cond: Iterable[Bool],
      pop_cond: Iterable[Bool],
      flush_cond: Bool,
      entry: Iterable[T]
  ): Unit = {
    require(push_cond.size == num_push_ports)
    require(pop_cond.size == num_pop_ports)

    val w_en = VecInit(Seq.fill(lane_width)(false.B))
    val w_data = VecInit(Seq.fill(lane_width)(0.U.asTypeOf(gen)))
    val w_addr = VecInit(Seq.fill(lane_width)(0.U(ptr_width.W)))

    // -------------------
    // push
    // -------------------
    for (i <- 0 until num_push_ports) {
      when(push_cond.toSeq(i)) {
        val current_push_ptr = push_ptrs(i)
        val l_idx = lane_idx(current_push_ptr)
        val l_addr = lane_addr(current_push_ptr)
        w_en(l_idx) := true.B
        w_data(l_idx) := entry.toSeq(i)
        w_addr(l_idx) := l_addr
      }
      when(w_en(i)) {
        lane_seq(i)(w_addr(i)) := w_data(i)
      }
    }

    // -------------------
    // pop
    // -------------------
    for (i <- 0 until num_pop_ports) {
      when(pop_cond.toSeq(i)) {
//        val current_pop_ptr = pop_ptrs(i)
//        if (with_valid) {
//          ram_valid(current_pop_ptr) := false.B
//        }
      }
    }

    // -----------------------
    // update ptr
    // -----------------------
    val push_count = PopCountOrder(push_cond)
    val pop_count = PopCountOrder(pop_cond)
    push_ptr_inc(push_count)
    pop_ptr_inc(pop_count)
    occupied_entries := occupied_entries + push_count - pop_count
    free_entries := free_entries - push_count + pop_count

    // -----------------------
    // flash
    // -----------------------
    flush(flush_cond)
    // -----------------------
    // assert
    // -----------------------
    assert(CheckOrder(push_cond), "push_cond must be ordered")
    assert(CheckOrder(pop_cond), "pop_cond must be ordered")
    assert(push_count <= free_entries, "push_cond should not overflow")
    assert(
      pop_count <= occupied_entries,
      "pop_cond should not overflow"
    )

    when(RegNext(flush_cond)) {
      assert(occupied_entries === 0.U, "num_counter should be zero after flush")
      assert(free_entries === size.U, "free_entries should be size after flush")
      assert(
        pop_ptrs === VecInit(
          Seq.tabulate(lane_width)(i => i.U(ptr_width.W))
        ),
        "pop_ptr_seq should be zero after flush"
      )
      assert(
        push_ptrs === VecInit(
          Seq.tabulate(lane_width)(i => i.U(ptr_width.W))
        ),
        "push_ptr_seq should be zero after flush"
      )
    }

    when(full) {
      assert(occupied_entries === size.U)
      assert(free_entries === 0.U)
    }
  }
}

class DummyMultiPortFIFO[T <: Data](
    gen: T,
    size: Int,
    num_push_ports: Int,
    num_pop_ports: Int
) extends Module {
  val io = IO(new Bundle {
    val in = Vec(num_push_ports, Flipped(Decoupled(gen)))
    val flush = Input(Bool())
    val out = Vec(num_pop_ports, Decoupled(gen))
  })

  val fifo = new MultiPortFIFOUseMEM(
    gen,
    size,
    num_push_ports,
    num_pop_ports
  )

  fifo.push_pop_flush_cond(
    VecInit(io.in.map(_.fire)),
    VecInit(io.out.map(_.fire)),
    io.flush,
    VecInit(io.in.map(_.bits))
  )

  val peek = fifo.peek()
//  require(peek.length == num_pop_ports)
  peek
    .take(num_pop_ports)
    .zipWithIndex
    .foreach({ case (p, i) =>
      io.out(i).bits := p.bits
      io.out(i).valid := p.valid
    })

  0.until(num_push_ports)
    .foreach({ i =>
      io.in(i).ready := fifo.free_entries > i.U
    })

  // --------------------------
  // formal
  // -------------------------
  val f_push_valid_order = CheckOrder(VecInit(io.in.map(_.valid)))
  val f_push_ready_order = CheckOrder(VecInit(io.in.map(_.ready)))
  val f_pop_valid_order = CheckOrder(VecInit(io.out.map(_.valid)))
  val f_pop_ready_order = CheckOrder(VecInit(io.out.map(_.ready)))
  val f_push_fire_order = CheckOrder(VecInit(io.in.map(_.fire)))
  val f_pop_fire_order = CheckOrder(VecInit(io.out.map(_.fire)))

  assert(f_push_valid_order)
  assert(f_push_ready_order)
  assert(f_pop_valid_order)
  assume(f_pop_ready_order)
  assert(f_push_fire_order)
  assert(f_pop_fire_order)
}

object gen_multi_port_valid_fifo_verilog extends App {
  GenVerilogHelper(new DummyMultiPortFIFO(UInt(32.W), 16, 4, 2))
}
