package leesum
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.Decoupled
import chiseltest._
import leesum.TestUtils.{check_aligned, gen_rand_uint, long2UInt64}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec

class LSUTestDut(memoryFIle: String = "") extends Module {
  val io = IO(new Bundle {
    val lsu_req = Flipped(Decoupled(new LSUReq))
    val flush = Input(Bool())
    // commit interface
    val mmio_commit = Flipped(Decoupled(Bool()))
    val store_commit = Flipped(Decoupled(Bool()))
    // write-back interface
    val lsu_resp = Decoupled(new LSUResp)
  })

  val lsu = Module(new LSU)
  val dcache = Module(new DummyDCache(memoryFIle))
  val tlb = Module(new DummyTLB())

  // flush
  lsu.io.flush := io.flush
  dcache.io.flush := io.flush
  tlb.io.flush := io.flush

  // lsu <> io
  lsu.io.lsu_req <> io.lsu_req
  lsu.io.mmio_commit <> io.mmio_commit
  lsu.io.store_commit <> io.store_commit
  lsu.io.lsu_resp <> io.lsu_resp

  // lsu <> dcache
  lsu.io.dcache_load_req <> dcache.io.load_req
  lsu.io.dcache_load_resp <> dcache.io.load_resp
  lsu.io.dcache_store_req <> dcache.io.store_req
  lsu.io.dcache_store_resp <> dcache.io.store_resp

  // lsu <> tlb
  lsu.io.tlb_req <> tlb.io.tlb_req
  lsu.io.tlb_resp <> tlb.io.tlb_resp

  // lsu <> lsu_resp
  lsu.io.lsu_resp <> io.lsu_resp

}

object gen_LSUTestDut_verilog extends App {
  GenVerilogHelper(new LSUTestDut)
}

class RefLSUMemory(ADDR_WIDTH: Int, DATA_WIDTH: Int, BASE_ADDR: Int) {
  require(DATA_WIDTH % 8 == 0, "DATA_WIDTH must be a multiple of 8")
  var mem = Array.fill(1 << ADDR_WIDTH)(0.toByte)

  def get_real_size(size: BigInt): Int = {
    size.toInt match {
      case 0 => 1
      case 1 => 2
      case 2 => 4
      case 3 => 8
    }
  }
  def get_real_addr(addr: BigInt): Int = {
    (addr - BASE_ADDR).toInt
  }
  def read(addr: BigInt, size: BigInt): BigInt = {
    require(
      check_aligned(addr.toLong, size.toInt),
      "RefLSUMemory must be aligned"
    )
    val real_size = get_real_size(size)
    val real_addr = get_real_addr(addr)

    val rdata = mem
      .slice(real_addr, real_addr + real_size)
    TestUtils.byteSeq2Uint64LittleEndian(rdata)
  }

  def write(addr: BigInt, size: BigInt, data: BigInt): Unit = {
    require(
      check_aligned(addr.toLong, size.toInt),
      "RefLSUMemory must be aligned"
    )
    val real_size = get_real_size(size)
    val real_addr = get_real_addr(addr)

    0.to(real_size).foreach { i =>
      mem(real_addr + i) = ((data >> (i * 8)) & 0xff).toByte
    }
  }
}

object test_run1 extends App {
  var mem = new RefLSUMemory(12, 64, 0)

  mem.write(0, 3, 0x123456789abcdefL)

  val byte_array = mem.read(0, 3).toByteArray

}

class LSUTest extends AnyFreeSpec with ChiselScalatestTester {

//  class AGUReq extends Bundle {
//    val op_a = UInt(64.W)
//    val op_b = UInt(64.W)
//    val size = UInt(2.W)
//    val store_data = UInt(64.W)
//    val trans_id = UInt(32.W)
//    val is_store = Bool()
//    // need by load
//    val sign_ext = Bool()
//  }
  def gen_lsu_req(
      op_a: Long,
      op_b: Long,
      size: Int,
      wdata: Long,
      trans_id: Long,
      is_store: Boolean,
      sign_ext: Boolean
  ) = {}

}
