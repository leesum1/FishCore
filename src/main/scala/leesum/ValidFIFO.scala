package leesum

import chisel3._
import chisel3.util.{Decoupled, Valid, isPow2, log2Ceil}

/** This is a FIFO, which use Mem as storage. And each element in the FIFO has a
  * valid bit. The FIFO has a push port and a pop port, and can read randomly
  * through the create_read_port API.
  * @param gen
  * @param size
  * @param name
  * @tparam T
  */
class ValidFIFO[T <: Data](
    gen: T,
    size: Int,
    name: String
) {
  require(isPow2(size), "content must have power-of-2 number of entries")

  private val content_data = Mem(size, gen).suggestName(name + "_data")
  private val content_valid =
    RegInit(VecInit(Seq.fill(size)(false.B))).suggestName(name + "_valid")

  val push_ptr = RegInit(
    0.U(log2Ceil(size).W)
  ).suggestName(name + "_push_ptr")
  val pop_ptr = RegInit(0.U(log2Ceil(size).W)).suggestName(name + "_pop_ptr")
  val num_counter = RegInit(0.U((log2Ceil(size) + 1).W))
  val free_entries = RegInit(size.U(log2Ceil(size + 1).W))

  val empty = num_counter === 0.U
  val full = num_counter(log2Ceil(size)) === 1.U
  val occupied_entries = num_counter

  empty.suggestName(name + "_empty")
  full.suggestName(name + "_full")
  free_entries.suggestName(name + "_free_entries")
  occupied_entries.suggestName(name + "_occupied_entries")

  private def read(ptr: UInt): Valid[T] = {
    assert(ptr < size.U, "ptr must less than size")
    val data = content_data(ptr)
    val valid = content_valid(ptr)
    val res = Wire(new Valid(gen))
    res.bits := data
    res.valid := valid
    res
  }
  private def write(ptr: UInt, valid: Bool, data: T): Unit = {
    assert(ptr < size.U, "ptr must less than size")
    content_data(ptr) := data
    content_valid(ptr) := valid
  }

  def create_read_port(addr: UInt): Valid[T] = {
    read(addr)
  }

  def peek(): Valid[T] = {
    read(pop_ptr)
  }

  def flush(flush_cond: Bool): Unit = {
    when(flush_cond) {
      push_ptr := 0.U
      pop_ptr := 0.U
      num_counter := 0.U
      free_entries := size.U
      for (i <- 0 until size) {
        content_valid(i) := false.B
      }
    }
  }

  def push_pop_flush_cond(
      push_cond: Bool,
      pop_cond: Bool,
      flush_cond: Bool,
      entry: T
  ): Unit = {
    when(push_cond) {
      content_data(push_ptr) := entry
      content_valid(push_ptr) := true.B

      push_ptr := push_ptr + 1.U
      num_counter := num_counter + 1.U
      free_entries := free_entries - 1.U
    }
    when(pop_cond) {
      content_valid(pop_ptr) := false.B
      pop_ptr := pop_ptr + 1.U
      num_counter := num_counter - 1.U
      free_entries := free_entries + 1.U
    }
    when(push_cond && pop_cond) {
      num_counter := num_counter
      free_entries := free_entries
    }
    flush(flush_cond)

    // -----------------------
    // assert
    // -----------------------

    when(RegNext(flush_cond)) {
      for (i <- 0 until size) {
        assert(
          !content_valid(i),
          "content must be invalid after flush"
        )
      }
    }
  }
}

/** This is a multi-port FIFO, which use Reg as storage. And each element in the
  * FIFO has a valid bit. And you can use create_read_port API to read
  * randomly.The flag signals(push_ptr, pop_ptr, num_counter, free_entries) were
  * all register.
  * @param gen
  * @param size
  * @param name
  * @param num_push_ports
  * @param num_pop_ports
  * @tparam T
  */
class MultiPortValidFIFO[T <: Data](
    gen: T,
    size: Int,
    name: String,
    num_push_ports: Int,
    num_pop_ports: Int
) {
  require(
    num_push_ports > 0,
    "MultiportFIFO must have non-zero number of push-ports"
  )
  require(
    num_pop_ports > 0,
    "MultiportFIFO must have non-zero number of pop-ports"
  )
  val content = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(new Valid(gen)))))

  val ptr_width = log2Ceil(size)

  val push_ptr_seq = RegInit(
    VecInit(Seq.tabulate(num_push_ports)(i => i.U(ptr_width.W)))
  )
  val pop_ptr_seq = RegInit(
    VecInit(Seq.tabulate(num_pop_ports)(i => i.U(ptr_width.W)))
  )

  private def push_ptr_inc(size: UInt): Unit = {
    assert(size <= num_push_ports.U, "size must less than num_push_ports")
    push_ptr_seq.foreach({ ptr =>
      ptr := ptr + size
    })
  }
  private def pop_ptr_inc(size: UInt): Unit = {
    assert(size <= num_pop_ports.U, "size must less than num_pop_ports")
    pop_ptr_seq.foreach({ ptr =>
      ptr := ptr + size
    })
  }

  val num_counter = RegInit(0.U((log2Ceil(size) + 1).W))
  val free_entries = RegInit(size.U(log2Ceil(size + 1).W))

  val empty = num_counter === 0.U
  val full = num_counter(log2Ceil(size)) === 1.U
  val occupied_entries = num_counter

  empty.suggestName(name + "_empty")
  full.suggestName(name + "_full")
  free_entries.suggestName(name + "_free_entries")
  occupied_entries.suggestName(name + "_occupied_entries")

  def peek(): Vec[Valid[T]] = {
    val last = Wire(Vec(num_pop_ports, Valid(gen)))
    for (i <- 0 until num_pop_ports) {
      val current_pop_ptr = pop_ptr_seq(i)
      last(i) := content(current_pop_ptr)
    }
    last
  }

  def create_read_port(addr: UInt): Valid[T] = {
    assert(addr < size.U, "addr must less than size")
    content(addr)
  }

  def flush(flush_cond: Bool): Unit = {
    when(flush_cond) {
      num_counter := 0.U
      free_entries := size.U
      push_ptr_seq.zipWithIndex.foreach({ case (ptr, i) =>
        ptr := i.U
      })
      pop_ptr_seq.zipWithIndex.foreach({ case (ptr, i) =>
        ptr := i.U
      })
      content.foreach({ v =>
        v.valid := false.B
      })
    }
  }

  def push_pop_flush_cond_multi_port(
      push_cond: Vec[Bool],
      pop_cond: Vec[Bool],
      flush_cond: Bool,
      entry: Vec[T]
  ): Unit = {

    require(push_cond.length == num_push_ports)
    require(pop_cond.length == num_pop_ports)
    require(entry.length == num_push_ports)
    // -------------------
    // push
    // -------------------
    for (i <- 0 until num_push_ports) {
      when(push_cond(i)) {
        val current_push_ptr = push_ptr_seq(i)
        content(current_push_ptr).valid := true.B
        content(current_push_ptr).bits := entry(i)
      }
    }
    // -------------------
    // pop
    // -------------------
    for (i <- 0 until num_pop_ports) {
      when(pop_cond(i)) {
        val current_pop_ptr = pop_ptr_seq(i)
        content(current_pop_ptr).valid := false.B
      }
    }

    // -----------------------
    // update ptr
    // -----------------------
    val push_count = PopCountOrder(push_cond)
    val pop_count = PopCountOrder(pop_cond)

    push_ptr_inc(push_count)
    pop_ptr_inc(pop_count)

    num_counter := num_counter + push_count - pop_count
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
      assert(num_counter === 0.U, "num_counter should be zero after flush")
      assert(free_entries === size.U, "free_entries should be size after flush")
      assert(
        pop_ptr_seq === VecInit(
          Seq.tabulate(num_pop_ports)(i => i.U(ptr_width.W))
        ),
        "pop_ptr_seq should be zero after flush"
      )
      assert(
        push_ptr_seq === VecInit(
          Seq.tabulate(num_push_ports)(i => i.U(ptr_width.W))
        ),
        "push_ptr_seq should be zero after flush"
      )
    }
  }
}

/** This is a multi-port FIFO, which use Reg as storage. And each element in the
  * FIFO has a valid bit. And you can use create_read_port API to read
  * randomly.The flag signals(push_ptr, pop_ptr, num_counter, free_entries) were
  * not all register.
  * @param gen
  * @param size
  * @param name
  * @param num_push_ports
  * @param num_pop_ports
  * @tparam T
  */
class MultiPortValidFIFO2[T <: Data](
    gen: T,
    size: Int,
    name: String,
    num_push_ports: Int,
    num_pop_ports: Int
) {
  require(
    num_push_ports > 0,
    "MultiportFIFO must have non-zero number of push-ports"
  )
  require(
    num_pop_ports > 0,
    "MultiportFIFO must have non-zero number of pop-ports"
  )
  val content = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(new Valid(gen)))))

  val ptr_width = log2Ceil(size)

  val push_ptr = RegInit(
    0.U(ptr_width.W)
  )
  val pop_ptr = RegInit(
    0.U(ptr_width.W)
  )

  val push_ptr_seq = VecInit(
    Seq.tabulate(num_push_ports)(i => push_ptr + i.U(ptr_width.W))
  )
  val pop_ptr_seq = VecInit(
    Seq.tabulate(num_pop_ports)(i => pop_ptr + i.U(ptr_width.W))
  )

  private def push_ptr_inc(size: UInt): Unit = {
    assert(size <= num_push_ports.U, "size must less than num_push_ports")
    push_ptr := push_ptr + size
  }
  private def pop_ptr_inc(size: UInt): Unit = {
    assert(size <= num_pop_ports.U, "size must less than num_pop_ports")
    pop_ptr := pop_ptr + size
  }

  val num_counter = RegInit(0.U((log2Ceil(size) + 1).W))
  val free_entries = RegInit(size.U(log2Ceil(size + 1).W))

  val empty = num_counter === 0.U
  val full = num_counter(log2Ceil(size)) === 1.U
  val occupied_entries = num_counter

  empty.suggestName(name + "_empty")
  full.suggestName(name + "_full")
  free_entries.suggestName(name + "_free_entries")
  occupied_entries.suggestName(name + "_occupied_entries")

  def peek(): Vec[Valid[T]] = {
    val last = Wire(Vec(num_pop_ports, Valid(gen)))
    for (i <- 0 until num_pop_ports) {
      val current_pop_ptr = pop_ptr_seq(i)
      last(i) := content(current_pop_ptr)
    }
    last
  }

  def create_read_port(addr: UInt): Valid[T] = {
    assert(addr < size.U, "addr must less than size")
    content(addr)
  }

  def flush(flush_cond: Bool): Unit = {
    when(flush_cond) {
      num_counter := 0.U
      free_entries := size.U
      push_ptr := 0.U
      pop_ptr := 0.U
      content.foreach({ v =>
        v.valid := false.B
      })
    }
  }

  def push_pop_flush_cond_multi_port(
      push_cond: Vec[Bool],
      pop_cond: Vec[Bool],
      flush_cond: Bool,
      entry: Vec[T]
  ): Unit = {

    require(push_cond.length == num_push_ports)
    require(pop_cond.length == num_pop_ports)
    require(entry.length == num_push_ports)
    // -------------------
    // push
    // -------------------
    for (i <- 0 until num_push_ports) {
      when(push_cond(i)) {
        val current_push_ptr = push_ptr_seq(i)
        content(current_push_ptr).valid := true.B
        content(current_push_ptr).bits := entry(i)
      }
    }
    // -------------------
    // pop
    // -------------------
    for (i <- 0 until num_pop_ports) {
      when(pop_cond(i)) {
        val current_pop_ptr = pop_ptr_seq(i)
        content(current_pop_ptr).valid := false.B
      }
    }

    // -----------------------
    // update ptr
    // -----------------------
    val push_count = PopCountOrder(push_cond)
    val pop_count = PopCountOrder(pop_cond)

    push_ptr_inc(push_count)
    pop_ptr_inc(pop_count)

    num_counter := num_counter + push_count - pop_count
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
      assert(num_counter === 0.U, "num_counter should be zero after flush")
      assert(free_entries === size.U, "free_entries should be size after flush")
      assert(
        pop_ptr_seq === VecInit(
          Seq.tabulate(num_pop_ports)(i => i.U(ptr_width.W))
        ),
        "pop_ptr_seq should be zero after flush"
      )
      assert(
        push_ptr_seq === VecInit(
          Seq.tabulate(num_push_ports)(i => i.U(ptr_width.W))
        ),
        "push_ptr_seq should be zero after flush"
      )
    }
  }
}

/** This is a multi-port FIFO, which use Mem as storage. And each element in the
  * FIFO has a valid bit.This MultiPortValidFIFO can only support push and pop
  * operation.Don't support random read or write.The push ports and pop ports
  * must be ordered(the later port can be set only if the early port was set).
  * @param gen
  * @param size
  * @param name
  * @param num_push_ports
  * @param num_pop_ports
  * @tparam T
  */
class MultiPortValidFIFOUseMem[T <: Data](
    gen: T,
    size: Int,
    name: String,
    num_push_ports: Int,
    num_pop_ports: Int
) {
  require(
    num_push_ports > 0,
    "MultiportFIFO must have non-zero number of push-ports"
  )
  require(
    num_pop_ports > 0,
    "MultiportFIFO must have non-zero number of pop-ports"
  )
  val ptr_width = log2Ceil(size)

  val fifo_valid = RegInit(VecInit(Seq.fill(size)(false.B)))
  val fifo_data = Mem(size, gen).suggestName(name + "_data")

  val push_ptr_seq = RegInit(
    VecInit(Seq.tabulate(num_push_ports)(i => i.U(ptr_width.W)))
  )
  val pop_ptr_seq = RegInit(
    VecInit(Seq.tabulate(num_pop_ports)(i => i.U(ptr_width.W)))
  )

  private def push_ptr_inc(size: UInt): Unit = {
    assert(size <= num_push_ports.U, "size must less than num_push_ports")
    push_ptr_seq.foreach({ ptr =>
      ptr := ptr + size
    })
  }
  private def pop_ptr_inc(size: UInt): Unit = {
    assert(size <= num_pop_ports.U, "size must less than num_pop_ports")
    pop_ptr_seq.foreach({ ptr =>
      ptr := ptr + size
    })
  }

  val num_counter = RegInit(0.U((log2Ceil(size) + 1).W))
  val free_entries = RegInit(size.U(log2Ceil(size + 1).W))

  val empty = num_counter === 0.U
  val full = num_counter(log2Ceil(size)) === 1.U
  val occupied_entries = num_counter

  empty.suggestName(name + "_empty")
  full.suggestName(name + "_full")
  free_entries.suggestName(name + "_free_entries")
  occupied_entries.suggestName(name + "_occupied_entries")

  def peek(): Vec[Valid[T]] = {
    val last = Wire(Vec(num_pop_ports, Valid(gen)))
    for (i <- 0 until num_pop_ports) {
      val current_pop_ptr = pop_ptr_seq(i)
      last(i).valid := fifo_valid(current_pop_ptr)
      last(i).bits := fifo_data(current_pop_ptr)
    }
    last
  }

  def flush(flush_cond: Bool): Unit = {
    when(flush_cond) {
      num_counter := 0.U
      free_entries := size.U
      push_ptr_seq.zipWithIndex.foreach({ case (ptr, i) =>
        ptr := i.U
      })
      pop_ptr_seq.zipWithIndex.foreach({ case (ptr, i) =>
        ptr := i.U
      })
      fifo_valid.foreach({ v =>
        v := false.B
      })
    }
  }

  def push_pop_flush_cond_multi_port(
      push_cond: Vec[Bool],
      pop_cond: Vec[Bool],
      flush_cond: Bool,
      entry: Vec[T]
  ): Unit = {

    require(push_cond.length == num_push_ports)
    require(pop_cond.length == num_pop_ports)
    require(entry.length == num_push_ports)
    // -------------------
    // push
    // -------------------
    for (i <- 0 until num_push_ports) {
      when(push_cond(i)) {
        val current_push_ptr = push_ptr_seq(i)
        fifo_valid(current_push_ptr) := true.B
        fifo_data(current_push_ptr) := entry(i)
      }
    }
    // -------------------
    // pop
    // -------------------
    for (i <- 0 until num_pop_ports) {
      when(pop_cond(i)) {
        val current_pop_ptr = pop_ptr_seq(i)
        fifo_valid(current_pop_ptr) := false.B
      }
    }

    // -----------------------
    // update ptr
    // -----------------------
    val push_count = PopCountOrder(push_cond)
    val pop_count = PopCountOrder(pop_cond)

    push_ptr_inc(push_count)
    pop_ptr_inc(pop_count)

    num_counter := num_counter + push_count - pop_count
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
      assert(num_counter === 0.U, "num_counter should be zero after flush")
      assert(free_entries === size.U, "free_entries should be size after flush")
      assert(
        pop_ptr_seq === VecInit(
          Seq.tabulate(num_pop_ports)(i => i.U(ptr_width.W))
        ),
        "pop_ptr_seq should be zero after flush"
      )
      assert(
        push_ptr_seq === VecInit(
          Seq.tabulate(num_push_ports)(i => i.U(ptr_width.W))
        ),
        "push_ptr_seq should be zero after flush"
      )
    }
  }
}

class DummyValidFIFO extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(64.W)))
    val flush = Input(Bool())
    val out = Decoupled(UInt(64.W))
  })

  val fifo = new ValidFIFO(UInt(64.W), 4, "leesum_fifo")

  fifo.push_pop_flush_cond(io.in.fire, io.out.fire, io.flush, io.in.bits)

  io.out.bits := fifo.peek().bits
  io.out.valid := fifo.peek().valid && !fifo.empty
  io.in.ready := !fifo.full
}

class DummyMultiPortValidFIFO[T <: Data](
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

  val fifo = new MultiPortValidFIFO(
    gen,
    size,
    "test_fifo",
    num_push_ports,
    num_pop_ports
  )

  fifo.push_pop_flush_cond_multi_port(
    VecInit(io.in.map(_.fire)),
    VecInit(io.out.map(_.fire)),
    io.flush,
    VecInit(io.in.map(_.bits))
  )

  val peek = fifo.peek()
  require(peek.length == num_pop_ports)
  peek.zipWithIndex.foreach({ case (p, i) =>
    io.out(i).bits := p.bits
    io.out(i).valid := p.valid
  })

  0.until(num_push_ports)
    .foreach({ i =>
      io.in(i).ready := fifo.free_entries > i.U
    })
}

object gen_valid_fifo_verilog extends App {
  GenVerilogHelper(new DummyValidFIFO)
}
object gen_multi_port_valid_fifo_verilog extends App {
  GenVerilogHelper(new DummyMultiPortValidFIFO(UInt(64.W), 8, 2, 2))
}
