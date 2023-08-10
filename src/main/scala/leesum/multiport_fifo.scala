package leesum

import chisel3._
import chisel3.experimental.{DataMirror, requireIsChiselType}
import chisel3.stage.ChiselStage
import chisel3.util.{Counter, Decoupled, isPow2, log2Ceil}

class MultiportFIFO[T <: Data](
    val gen: T,
    val entries: Int,
    val num_readports: Int,
    val num_writeports: Int
)(implicit compileOptions: chisel3.CompileOptions)
    extends Module() {
  require(
    entries > 0,
    "MultiportFIFO must have non-zero number of entries"
  )
  require(
    isPow2(entries),
    "MultiportFIFO must have power-of-2 number of entries"
  )
  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val io = IO(
    new MultiportFIFOIO(genType, entries, num_readports, num_writeports)
  )

  val ram = Mem(entries, genType)

  val count_width = log2Ceil(entries) + 1
  val item_count = RegInit(0.U(count_width.W))
  val item_free = RegInit(entries.U(count_width.W))

  val enq_ptr = RegInit(0.U(log2Ceil(entries).W))
  val deq_ptr = RegInit(0.U(log2Ceil(entries).W))

  val full = item_count === entries.U && item_free === 0.U
  val empty = item_count === 0.U && item_free === entries.U

  item_free := item_free - 1.U
  item_count := item_count + 1.U

}

class MultiportFIFOIO[T <: Data](
    genType: T,
    entries: Int,
    num_readports: Int,
    num_writeports: Int
) extends Bundle {
  val enq = Flipped(Decoupled(Vec(num_writeports, genType)))
  val deq = Decoupled(Vec(num_readports, genType))
}
object gen_multiport_fifo_verilog extends App {
  val projectDir = System.getProperty("user.dir")

  val verilogDir = s"$projectDir/gen_verilog"
  println(s"verilogDir: $verilogDir")
  val stage = new ChiselStage()
    .emitVerilog(
      new MultiportFIFO(UInt(32.W), 8, 4, 4),
      Array("--target-dir", verilogDir)
    )
}
