//package leesum.axi4
//import Chisel.{Counter, switch}
//import chisel3._
//import chisel3.stage.ChiselStage
//import chisel3.util.{Decoupled, Enum, PopCount, Reverse, is}
//class AXI4Memory extends Module {
//  val io = IO(new AXISlaveIO(32, 64))
//  io.clear()
//
//  val mem = SyncReadMem(1024, UInt(64.W))
//
//  def idx_of_mem(addr: UInt): UInt = {
//    addr(9, 3)
//  }
//  def idx_of_word(addr: UInt): UInt = {
//    addr(2, 0)
//  }
//  def read_mem(addr: UInt): UInt = {
//    mem.read(idx_of_mem(addr))
//  }
//
//  val sIdle :: sRFirst :: sRBurst :: Nil = Enum(3)
//  val state = RegInit(sIdle)
//
//  val ar_buf = RegInit(0.U.asTypeOf(new AXIAddressChannel(32)))
//  val burst_count = Counter(8)
//
//  def axi_read(): Unit = {
//    io.r.valid := true.B
//    io.r.bits.last := burst_count.value === ar_buf.len
//    io.r.bits.resp := AXIConst.RESP_OKAY
//    io.r.bits.data := mem.read()
//  }
//
//  switch(state) {
//    // 用于接受 ar 地址，缓存地址
//    is(sIdle) {
//      when(io.ar.fire) {
//        state := sRFirst
//        ar_buf := io.ar.bits
//        burst_count.reset()
//      }
//    }
//    // ar 地址缓存完毕，开始第一次读取
//    is(sRFirst) {
//      io.r.valid := true.B
//      io.r.bits.last := burst_count.value === ar_buf.len
//      io.r.bits.resp := AXIConst.RESP_OKAY
//      io.r.bits.data := mem.read(ar_buf.addr + burst_count.value)
//      when(io.r.fire) {
//        burst_count.inc()
//        when(burst_count.value === ar_buf.len) {
//          state := sIdle
//        }
//      }
//    }
//
//  }
//}
//
//object gen_alu_verilog extends App {
//  val projectDir = System.getProperty("user.dir")
//
//  val verilogDir = s"$projectDir/gen_verilog"
//  println(s"verilogDir: $verilogDir")
//  val stage = new ChiselStage()
//    .emitVerilog(
//      new AXI4Memory(),
//      Array("--target-dir", verilogDir)
//    )
//}
