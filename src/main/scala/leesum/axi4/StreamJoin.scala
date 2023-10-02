package leesum.axi4
import chisel3._
import chisel3.util._
import leesum.GenVerilogHelper
//class DecoupledIOJoin[T <: Data](gen: T) extends Module {}
class JoinBundle[T1 <: Data, T2 <: Data](gen1: T1, gen2: T2) extends Bundle {
  val element1 = gen1
  val element2 = gen2

}
object StreamJoin {

  /** Join two streams into a stream of tuples
    */
  def apply[T1 <: Data, T2 <: Data](
      source1: DecoupledIO[T1],
      source2: DecoupledIO[T2],
      source1_cond: Bool = true.B,
      source2_cond: Bool = true.B
  ): DecoupledIO[JoinBundle[T1, T2]] = {
    val s = Wire(
      Decoupled(new JoinBundle(source1.bits.cloneType, source2.bits.cloneType))
    )

    val source1_valid = Mux(source1_cond, source1.valid, true.B)
    val source2_valid = Mux(source2_cond, source2.valid, true.B)

    s.valid := source1_valid && source2_valid
    source1.ready := s.ready
    source2.ready := s.ready
    s.bits.element1 := source1.bits
    s.bits.element2 := source2.bits
    s
  }

}

class DummyStreamJoin extends Module {
  val io = IO(new Bundle {
    val in1 = Flipped(Decoupled(UInt(2.W)))
    val in2 = Flipped(Decoupled(UInt(64.W)))
    val out = Decoupled(new JoinBundle(UInt(2.W), UInt(64.W)))
  })

  io.out <> StreamJoin(io.in1, io.in2)
}

object gen_stream_join_verilog extends App {
  GenVerilogHelper(new DummyStreamJoin)
}
