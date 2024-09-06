package leesum.Utils

import chisel3._

object SimLog {
  val enable = true
  def _base_log(fmt: String, data: Bits*) = {
    if (enable) {
      printf(fmt, data: _*)
    }
  }
  def apply(name: String, fmt: String, data: Bits*) = {
    custom(name, fmt, data: _*)
  }

  def custom(name: String, fmt: String, data: Bits*) = {
    _base_log(s"[Chisel][$name]:" + fmt, data: _*)
  }
  def info(fmt: String, data: Bits*) = {
    custom("INFO", fmt, data: _*)
  }
  def warn(fmt: String, data: Bits*) = {
    custom("WARN", fmt, data: _*)
  }
  def error(fmt: String, data: Bits*) = {
    custom("ERROR", fmt, data: _*)
  }
}
