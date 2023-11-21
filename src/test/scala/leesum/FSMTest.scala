//package leesum
//
//import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
//import chisel3.util.experimental.decode.{TruthTable, decoder}
//import chisel3.util.{BitPat, Decoupled, Enum, is, switch}
//import chisel3.{ActualDirection, _}
//import chiseltest._
//import org.scalacheck.Gen
//import org.scalatest.freespec.AnyFreeSpec
//
//class fsm_test extends Module {
//  val io = IO(new Bundle {
//    val in_valid = Input(Bool())
//    val in_data = Input(UInt(4.W))
//    val wire_out = Output(UInt(4.W))
//    val reg_out = Output(UInt(4.W))
//  })
//
//  val sIdle :: sFirst :: Nil = Enum(2)
//  val state = RegInit(sIdle)
//  val wire_a = WireInit(0.U(4.W))
//  val reg_a = RegInit(0.U(4.W))
//
//  // before switch, wire_a := 0.U, reg_a := 0.U
//  reg_a := 0.U
//  wire_a := 0.U
//  switch(state) {
//    is(sIdle) {
//      when(io.in_valid) {
//        state := sFirst
//        wire_a := io.in_data + 1.U
//        reg_a := io.in_data + 1.U
//      }
//    }
//    is(sFirst) {
//      when(io.in_valid) {
//        state := sIdle
//        wire_a := io.in_data + 2.U
//        reg_a := io.in_data + 2.U
//      }
//    }
//  }
//  io.wire_out := wire_a
//  io.reg_out := reg_a
//}
//
//class FSMDut2Req extends Bundle {
//  val data = UInt(8.W)
//  val op = UInt(3.W)
//}
//
//class FSMDut2 extends Module {
//  val io = IO(new Bundle {
//    val req = Flipped(Decoupled(new FSMDut2Req))
//    val resp = Decoupled(UInt(8.W))
//    val op1 = Flipped(Decoupled(Bool()))
//    val op2 = Flipped(Decoupled(Bool()))
//  })
//
//  val req_queue = new ValidFIFO(new FSMDut2Req, 4, "req_queue")
//
//  val pop_cond = WireInit(false.B)
//
//  req_queue.push_pop_flush_cond(
//    io.req.fire,
//    pop_cond,
//    false.B,
//    io.req.bits
//  )
//
//  io.req.ready := !req_queue.full
//
//  val sIdle :: sFirst :: sSecond :: Nil = Enum(3)
//
//  def send_resp(data: UInt) = {
//    io.resp.valid := true.B
//    io.resp.bits := data
//  }
//
//  def fsm_op1(req: FSMDut2Req) = {
//    val op1_state = RegInit(sIdle)
//    switch(op1_state) {
//      is(sIdle) {
//        assert(req.op === 0.U)
//        io.op1.ready := req_queue.peek().valid
//        when(io.op1.fire) {
//          op1_state := sFirst
//        }
//      }
//      is(sFirst) {
//        op1_state := sSecond
//      }
//      is(sSecond) {
//        io.resp.valid := true.B
//        io.resp.bits := req.data + 1.U
//        when(io.resp.fire) {
//          op1_state := sIdle
//          pop_cond := true.B
//        }
//      }
//    }
//  }
//
//  def fsm_op2(req: FSMDut2Req) = {
//    val op2_state = RegInit(sIdle)
//    switch(op2_state) {
//      is(sIdle) {
//        assert(req.op === 1.U)
//        io.op2.ready := req_queue.peek().valid
//        when(io.op2.fire) {
//          op2_state := sFirst
//        }
//      }
//      is(sFirst) {
//        op2_state := sSecond
//      }
//      is(sSecond) {
//        io.resp.valid := true.B
//        io.resp.bits := req.data + 2.U
//        when(io.resp.fire) {
//          op2_state := sIdle
//          pop_cond := true.B
//        }
//      }
//    }
//  }
//
//  io.op1.nodeq()
//  io.op2.nodeq()
//  io.resp.noenq()
//
//  val peek = req_queue.peek()
//
//  when(peek.valid) {
//    val req = peek.bits
//    switch(req.op) {
//      is(0.U) {
//        fsm_op1(req)
//      }
//      is(1.U) {
//        fsm_op2(req)
//      }
//    }
//  }
//}
//
//object gen_fsm_test2_verilog extends App {
//  GenVerilogHelper(new FSMDut2)
//}
//
//class FSMTest extends AnyFreeSpec with ChiselScalatestTester {
//
//  "fsm_test2" in {
//    test(new FSMDut2)
//      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) {
//        dut =>
//          // ----------------------------
//          // init port
//          // ----------------------------
//          dut.io.req.initSource().setSourceClock(dut.clock)
//          dut.io.op1.initSource().setSourceClock(dut.clock)
//          dut.io.op2.initSource().setSourceClock(dut.clock)
//          dut.io.resp.initSink().setSinkClock(dut.clock)
//
//          // ----------------------------
//          // prepare test data
//          // ----------------------------
//
//          def gen_req() = {
//            for {
//              data <- Gen.choose(0, 100)
//              op <- Gen.choose(0, 1)
//            } yield {
//              (new FSMDut2Req).Lit(_.data -> data.U, _.op -> op.U)
//            }
//          }
//          val req_seq = Gen.listOfN(1000, gen_req()).sample.get
//          val op1_count = req_seq.count(_.op.litValue == 0)
//          val op2_count = req_seq.count(_.op.litValue == 1)
//
//          val resp_seq = req_seq.map(req => {
//            (req.data.litValue + req.op.litValue + 1).U
//          })
//
//          // ----------------------------
//          // test1
//          // ----------------------------
//
//          fork {
//            req_seq.foreach(req => {
//              dut.io.req.enqueue(req)
//              dut.clock.step(Gen.chooseNum(1, 10).sample.get)
//            })
//          }.fork {
//            Seq
//              .fill(op1_count)(true.B)
//              .foreach(op1_en => {
//                dut.io.op1.enqueue(op1_en)
//                dut.clock.step(Gen.chooseNum(1, 10).sample.get)
//              })
//          }.fork {
//            Seq
//              .fill(op2_count)(true.B)
//              .foreach(op2_en => {
//                dut.io.op2.enqueue(op2_en)
//                dut.clock.step(Gen.chooseNum(1, 10).sample.get)
//              })
//          }.fork {
//            resp_seq.foreach(resp => {
//              dut.io.resp.expectDequeue(resp)
//              dut.clock.step(Gen.chooseNum(1, 10).sample.get)
//            })
//          }.joinAndStep(dut.clock)
//
//          // ----------------------------
//          // test2
//          // ----------------------------
//
//          fork {
//            dut.io.req.enqueueSeq(req_seq)
//          }.fork {
//            dut.io.op1.enqueueSeq(Seq.fill(op1_count)(true.B))
//          }.fork {
//            dut.io.op2.enqueueSeq(Seq.fill(op2_count)(true.B))
//          }.fork {
//            dut.io.resp.expectDequeueSeq(resp_seq)
//          }.joinAndStep(dut.clock)
//
//      }
//  }
//
//  "fsm_test" in {
//    test(new fsm_test)
//      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteFstAnnotation)) {
//        dut =>
//          {
//            dut.clock.step(5)
//            dut.io.in_valid.poke(true.B)
//            dut.io.in_data.poke(1.U)
//            // state idle
//            dut.io.wire_out.expect((1 + 1).U)
//            dut.io.reg_out.expect(0.U)
//
//            dut.clock.step(1)
//            dut.io.in_data.poke(2.U)
//            // state idle
//            dut.io.wire_out.expect((2 + 2).U)
//            dut.io.reg_out.expect((1 + 1).U)
//
//            dut.clock.step(1)
//            dut.io.in_valid.poke(false.B)
//
//            dut.io.reg_out.expect((2 + 2).U)
//            dut.io.wire_out.expect((0).U)
//
//            dut.clock.step(5)
//
//          }
//      }
//  }
//}
//class SimpleDecoder extends Module {
//  val table = TruthTable(
//    Map(
//      BitPat("b001") -> BitPat("b?"),
//      BitPat("b010") -> BitPat("b?"),
//      BitPat("b100") -> BitPat("b1"),
//      BitPat("b101") -> BitPat("b1"),
//      BitPat("b111") -> BitPat("b1")
//    ),
//    BitPat("b0")
//  )
//  val input = IO(Input(UInt(3.W)))
//  val output = IO(Output(UInt(1.W)))
//  output := decoder(input, table)
//}
//
//object gen_simple_decoder_verilog extends App {
//  GenVerilogHelper(new SimpleDecoder)
//}
