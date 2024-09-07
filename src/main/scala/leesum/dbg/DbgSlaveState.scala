package leesum.dbg
import chisel3._

class DbgSlaveState extends Bundle {
  // 临时的状态位，dm 只负责置 1
  val resumereq_flag = Bool()
  // 临时的状态位, 当 resume 时, 检查 dcsr 中的 step 位，如果为1，则置 1
  // 单步执行完毕后，清零
  val stepi_exec_flag = Bool()
  // 单步执行完毕后，置 1，此时处理器进入 debug 状态
  val stepi_redebug_flag = Bool()

  // 由 dm 控制置 1 或者清零
  val resetreq_signal = Bool()
  // 由 dm 控制置 1 或者清零
  val haltreq_signal = Bool()
  // 发送 resumereq 时，resumeack 首先被置为0, 进入 resume 状态
  // 处理器 resume 时，resumeack 被置为1
  val resumeack_signal = Bool()
  // 处理器复位时，havereset被置为1
  val havereset_signal = Bool()

  // 处理器是否处于 halt 状态
  val is_halted = Bool()

  // 接口辅助函数

  def set_stepi(): Unit = {
    // 下一次需要单步执行
    stepi_exec_flag := true.B
    stepi_redebug_flag := false.B
  }

  def clear_stepi(): Unit = {
    // 清零单步执行标志, 和进入单步执行后进入 debug 的状态标志
    stepi_exec_flag := false.B
    stepi_redebug_flag := false.B
  }

  def exec_stepi(): Unit = {
    // 单步执行完毕，需要重新进入 debug 状态
    stepi_exec_flag := false.B
    stepi_redebug_flag := true.B
  }

  def is_stepi_req(): Bool = {
    stepi_exec_flag && !stepi_redebug_flag
  }

  def set_haltreq(new_value: Bool): Unit = {
    haltreq_signal := new_value
  }

  def set_resumereq(): Unit = {
    resumereq_flag := true.B
    resumeack_signal := false.B
  }
  def halted(): Bool = {
    is_halted
  }

  def running(): Bool = {
    !is_halted
  }

  def resumeack(): Bool = {
    resumeack_signal
  }
  def set_reset_req(new_value: Bool): Unit = {
    resetreq_signal := new_value
  }
  def have_reset(): Bool = {
    havereset_signal
  }
  def clear_havereset(): Unit = {
    havereset_signal := false.B
  }
}
