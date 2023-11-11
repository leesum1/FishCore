//package leesum
//
//import chisel3._
//import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
//import chiseltest._
//import chiseltest.simulator.WriteVcdAnnotation
//import leesum.TestUtils.{byteSeq2Uint64LittleEndian, long2UInt64, long2Ulong}
//import net.fornwall.jelf
//import org.scalatest.freespec.AnyFreeSpec
//
//import java.math.BigInteger
//import java.nio.file.{Files, Paths}
//import scala.collection.mutable
//
//class RspPacket2Insts_test extends AnyFreeSpec with ChiselScalatestTester {
//
//  def gen_RspPacket(pc: BigInt, insts: BigInt): RspPacket = {
//    (new RspPacket).Lit(
//      _.pc -> long2UInt64(pc.toLong),
//      _.payload -> long2UInt64(insts.toLong)
//    )
//  }
//
//  "RspPacket2Insts_test" in {
//    test(new InstReAlign).withAnnotations(
//      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
//    ) { dut =>
//      {
//        // -----------------------
//        // init port
//        // -----------------------
//        dut.unaligned_insts.initSource().setSourceClock(dut.clock)
//        dut.output.initSink().setSinkClock(dut.clock)
//        dut.flush.poke(false.B)
//
//        // -----------------------
//        // prepare test data
//        // -----------------------
//
//        /** In this test, we read a elf file, then get the function's data get
//          * insts from the function's data, then compare the insts with dut's
//          * output
//          */
//        val filename =
//          "/home/leesum/workhome/riscv64-emu/ready_to_run/linux.elf"
//        val content = Files.readAllBytes(Paths.get(filename))
//        val elfFile = jelf.ElfFile.from(content)
//
//        // Find the section containing the function
//        val symbol = elfFile.getELFSymbol("sbi_init");
//
//        val entry_point = symbol.st_value;
//
//        val segment = (0 until elfFile.e_phnum)
//          .map(elfFile.getProgramHeader)
//          .filter(_.p_type == jelf.ElfSegment.PT_LOAD)
//          .filter(seg => {
//            (entry_point >= seg.p_vaddr) && (entry_point < seg.p_vaddr + seg.p_memsz)
//          })
//          .head
//
//        val offset = entry_point - segment.p_vaddr + segment.p_offset
//
//        val func_data =
//          content.slice(offset.toInt, offset.toInt + symbol.st_size.toInt)
//
//        val inst_pak = func_data
//          .grouped(2)
//          .map(group => {
//
//            new BigInteger(1, group.reverse).intValue()
//          })
//          .toArray
//
//        var idx = 0
//        var last_valid = false
//        var last_inst = 0
//
//        var ref_inst_list = mutable.Queue[UInt]()
//        var ref_pc_list = mutable.Queue[UInt]()
//        while (idx < inst_pak.size) {
//          // 32 bit inst
//          if (last_valid) {
//            val inst = (inst_pak(idx) & 0xffff) << 16 | last_inst
//            last_valid = false
////            println(inst)
//            ref_inst_list :+= ("x" + inst.toHexString).U
//            ref_pc_list :+= ("x" + (entry_point + idx * 2 - 2).toHexString).U
//
//          } else {
//            // 16 bit inst
//            if ((inst_pak(idx) & 0x03) != 0x03) {
////              println(inst_pak(idx))
//              ref_inst_list :+= ("x" + inst_pak(idx).toHexString).U
//              ref_pc_list :+= ("x" + (entry_point + idx * 2).toHexString).U
//            }
//            // the low 16bit of 32 bit inst
//            else {
//              last_valid = true
//              last_inst = inst_pak(idx) & 0xffff
//            }
//          }
//          idx += 1
//        }
//
//        dut.clock.step(10)
//
//        // all data is ready, start to test
//        val input_seq = func_data
//          .grouped(8)
//          .toList
//          .dropRight(
//            1
//          )
//          .map(byteSeq2Uint64LittleEndian(_))
//          .zipWithIndex
//          .map({ case (data, idx) =>
//            gen_RspPacket(entry_point + idx * 8, data)
//          })
//
//        // -----------------------
//        // start test
//        // -----------------------
//        dut.output.ready.poke(true.B)
//        input_seq.foreach(in => {
//          dut.unaligned_insts.enqueue(in)
//
//          val out = dut.output.peek()
//          if (out.valid.litToBoolean) {
//            out.bits.insts_vec.zipWithIndex.foreach(
//              { case (inst, idx) =>
//                if (inst.valid.litToBoolean) {
//                  dut.output.bits
//                    .insts_vec(idx)
//                    .pc
//                    .expect(ref_pc_list.dequeue)
//                  dut.output.bits
//                    .insts_vec(idx)
//                    .inst
//                    .expect(ref_inst_list.dequeue)
//                }
//              }
//            )
//          }
//        })
//
//        dut.clock.step()
//      }
//    }
//  }
//}
