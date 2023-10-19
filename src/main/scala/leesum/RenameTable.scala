package leesum
import chisel3._
import chisel3.util.{Valid, isPow2, log2Ceil}

class RenameSetPort(rob_nums: Int) extends Bundle {
  val rob_ptr_width = log2Ceil(rob_nums)
  val rd_addr = UInt(5.W)
  val rob_ptr = UInt(rob_ptr_width.W)
}

class RenameClearPort(rob_nums: Int) extends RenameSetPort(rob_nums) {}

class RenameTable(set_ports_num: Int, clear_ports_num: Int, rob_nums: Int) {
  require(isPow2(rob_nums), "rob_nums must be power of 2")
  val rob_ptr_width = log2Ceil(rob_nums)

  private val rename_table_valid = RegInit(
    VecInit(Seq.fill(32)(false.B))
  )
  private val rename_table_data = Mem(32, UInt(rob_ptr_width.W))

  private def read(addr: UInt): Valid[UInt] = {
    assert(addr < 32.U, "addr must less than 32")
    val valid = rename_table_valid(addr)
    val data = rename_table_data(addr)

    val res = Wire(Valid(UInt(rob_ptr_width.W)))
    res.valid := valid
    res.bits := data
    res
  }

  private def set(set_port: RenameSetPort) = {
    rename_table_valid(set_port.rd_addr) := true.B
    rename_table_data(set_port.rd_addr) := set_port.rob_ptr
  }
  private def clear(clear_port: RenameClearPort) = {
    val rd_addr = clear_port.rd_addr
    when(rename_table_data(rd_addr) === clear_port.rob_ptr) {
      rename_table_valid(rd_addr) := false.B
    }
  }

  private def flush() = {
    for (i <- 0 until 32) {
      rename_table_valid(i) := false.B
    }
  }

  def rand_access(addr: UInt): Valid[UInt] = {
    read(addr)
  }

  def set_clear_flush(
      set_ports: Vec[Valid[RenameSetPort]],
      clear_ports: Vec[Valid[RenameClearPort]],
      flush_port: Bool
  ) = {
    require(set_ports.length == this.set_ports_num)
    require(set_ports.length == this.clear_ports_num)

    // clear has the lowest priority
    // if set and clear the same rd_addr, clear will be ignored
    for (i <- 0 until clear_ports.length) {
      when(clear_ports(i).valid) {
        clear(clear_ports(i).bits)
      }
    }

    // if multiport set the same rd_addr, the last one will be set (WAW)
    for (i <- 0 until set_ports.length) {
      when(set_ports(i).valid) {
        set(set_ports(i).bits)
      }
    }

    when(flush_port) {
      flush()
    }

    // ----------------------
    // assert
    // ----------------------

    when(RegNext(flush_port)) {
      for (i <- 0 until 32) {
        assert(
          !rename_table_valid(i),
          "rename_table must be invalid after flush"
        )
      }
    }

    for (i <- 0 until set_ports.length) {
      when(set_ports(i).valid) {
        assert(
          set_ports(i).bits.rd_addr =/= 0.U,
          "set_ports bits must not be zero"
        )
        assert(
          set_ports(i).bits.rd_addr < 32.U,
          "addr must less than rob_nums"
        )
        assert(
          set_ports(i).bits.rob_ptr < rob_nums.U
        )
      }
    }

    for (i <- 0 until clear_ports.length) {
      when(clear_ports(i).valid) {
        assert(
          clear_ports(i).bits.rd_addr =/= 0.U,
          "clear_ports bits must not be zero"
        )
        assert(
          clear_ports(i).bits.rd_addr < 32.U,
          "addr must less than rob_nums"
        )
        assert(
          clear_ports(i).bits.rob_ptr < rob_nums.U
        )
      }
    }
  }
}

object gen_rename_table_verilog extends App {
  GenVerilogHelper(new Module {
    val io = IO(new Bundle {
      val set_ports = Vec(4, Flipped(Valid(new RenameSetPort(8))))
      val clear_ports = Vec(4, Flipped(Valid(new RenameClearPort(8))))
      val addr = Input(UInt(5.W))
      val rob_ptr = Output(Valid(UInt(3.W)))
      val flush = Input(Bool())
    })
    val rename_table = new RenameTable(4, 4, 8)
    rename_table.set_clear_flush(
      set_ports = io.set_ports,
      clear_ports = io.clear_ports,
      flush_port = io.flush
    )
    io.rob_ptr := rename_table.rand_access(io.addr)
  })
}
