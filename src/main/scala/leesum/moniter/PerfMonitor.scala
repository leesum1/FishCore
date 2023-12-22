package leesum.moniter
import chisel3._
class PerfMonitorCounter extends Bundle {
  val hit_counter = UInt(64.W)
  val num_counter = UInt(64.W)
  def reset(): Unit = {
    hit_counter := 0.U
    num_counter := 0.U
  }
  def inc_hit(c: UInt): Unit = {
    hit_counter := hit_counter + c
    num_counter := num_counter + 1.U
  }
  def inc_miss(c: UInt): Unit = {
    num_counter := num_counter + 1.U
  }
}
