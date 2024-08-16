package leesum.dbg
import chisel3._

class DbgSlaveState extends Bundle {
  // 临时的状态位，dm 只负责置 1
  val resumereq_flag = Bool()
  // 临时的状态位, 当 resume 时, 检查 dcsr 中的 step 位，如果为1，则置 1
  // 单步执行完毕后，清零
  val singlestep_execute_flag = Bool()
  // 单步执行完毕后，置 1，此时处理器进入 debug 状态
  val singlestep_debug_flag = Bool()

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

  def set_step(): Unit = {
    singlestep_execute_flag := true.B
    singlestep_debug_flag := false.B
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
