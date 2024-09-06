package leesum

import chisel3._
import chisel3.util.DecoupledIO
import chiseltest.formal.past

import scala.language.experimental.macros

object FormalUtils {

  /** Stability properties--what happens if valid and not ready , Assume any
    * response from the bus will not change prior to that response being
    * accepted
    *
    * @param io
    * @tparam T
    */
  def StreamShouldStable[T <: chisel3.Data](
      io: DecoupledIO[T]
  ): Bool = {
    past(io.valid && !io.ready)
  }

}
