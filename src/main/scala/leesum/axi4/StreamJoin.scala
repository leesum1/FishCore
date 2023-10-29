package leesum.axi4
import chisel3._
import chisel3.util._
import leesum.GenVerilogHelper

/** This module joins (num) streams into a stream, the payload was
  * ignored.Because we can use the payload form the source stream directly.
  * @param num
  */
class StreamJoin(num: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(num, Flipped(Decoupled(Bool())))
    val out = Decoupled(Bool())
  })
  io.out.valid := io.in.map(_.valid).reduce(_ && _)
  io.out.bits := io.in.map(_.bits).reduce(_ ^ _) // not used, just for test
  io.in.foreach(_.ready := io.out.fire)
}

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

object gen_stream_join_verilog extends App {
  GenVerilogHelper(new StreamJoin(2))
}
