package leesum

import chisel3._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import net.fornwall.jelf
import org.scalatest.freespec.AnyFreeSpec

import java.math.BigInteger
import java.nio.file.{Files, Paths}

class RspPacket2Insts_test extends AnyFreeSpec with ChiselScalatestTester {

  "RspPacket2Insts_test" in {
    test(new RspPacket2Insts).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      {
        dut.input.initSource()
        dut.input.setSourceClock(dut.clock)

        /** In this test, we read a elf file, then get the function's data get
          * insts from the function's data, then compare the insts with dut's
          * output
          */
        val filename =
          "/home/leesum/workhome/riscv64-emu/ready_to_run/linux.elf"
        val content = Files.readAllBytes(Paths.get(filename))
        val elfFile = jelf.ElfFile.from(content)

        // Find the section containing the function
        val symbol = elfFile.getELFSymbol("sbi_init");

        val entry_point = symbol.st_value;

//        elfFile.e_phnum

        val segment = (0 until elfFile.e_phnum)
          .map(elfFile.getProgramHeader)
          .filter(_.p_type == jelf.ElfSegment.PT_LOAD)
          .filter(seg => {
            (entry_point >= seg.p_vaddr) && (entry_point < seg.p_vaddr + seg.p_memsz)
          })
          .head

        val offset = entry_point - segment.p_vaddr + segment.p_offset

        val func_data =
          content.slice(offset.toInt, offset.toInt + symbol.st_size.toInt)

        val inst_pak = func_data
          .grouped(2)
          .map(group => {

            new BigInteger(1, group.reverse).intValue()
          })
          .toArray

        var idx = 0
        var last_valid = false
        var last_inst = 0

        var ref_inst_list = List[UInt]()
        var ref_pc_list = List[UInt]()
        while (idx < inst_pak.size) {
          // 32 bit inst
          if (last_valid) {
            val inst = (inst_pak(idx) & 0xffff) << 16 | last_inst
            last_valid = false
//            println(inst)
            ref_inst_list :+= ("x" + inst.toHexString).U
            ref_pc_list :+= ("x" + (entry_point + idx * 2 - 2).toHexString).U

          } else {
            // 16 bit inst
            if ((inst_pak(idx) & 0x03) != 0x03) {
//              println(inst_pak(idx))
              ref_inst_list :+= ("x" + inst_pak(idx).toHexString).U
              ref_pc_list :+= ("x" + (entry_point + idx * 2).toHexString).U
            }
            // the low 16bit of 32 bit inst
            else {
              last_valid = true
              last_inst = inst_pak(idx) & 0xffff
            }
          }
          idx += 1
        }

        dut.clock.step(10)

        // all data is ready, start to test
        func_data
          .grouped(8)
          .toList
          .dropRight(
            1
          ) // drop the last 8 group, because it may less than 8 byte
          .map((group: Array[Byte]) => {
            // convert 8 byte array to long(8 bytes), then to hex string
            var hex_string: String =
              "x" + new BigInteger(1, group.reverse).longValue().toHexString
            hex_string
          })
          .zipWithIndex
          .foreach {
            case (hex_string, foreach_idx) => {
              // the 8 byte data is ready, poke it to dut
//              println("---%s---".format(hex_string))

              dut.input.valid.poke(true.B)

              // because of chisel doesn't support convert a negative number to UInt
              // so we first convert Long to hex string(with prefix x)
              // then convert hex string to UInt(64.W) to avoid error
              dut.input.bits.payload.poke(hex_string.U)
              // poke pc
              dut.input.bits.pc.poke((entry_point + foreach_idx * 8).U)

              // check dut outputs
              for (i <- 0 until dut.input.bits.max_inst_packet) {
                val out = dut.output.insts_vec(i).peek();
                if (out.valid.litToBoolean) {
//
//                  println(
//                    "dut: pc:%x, inst:%08x".format(
//                      out.pc.litValue.toLong,
//                      out.inst.litValue.toLong
//                    )
//                  )
//                  println(
//                    "ref: pc:%x, inst:%08x".format(
//                      ref_pc_list.head.litValue.toLong,
//                      ref_inst_list.head.litValue.toLong
//                    )
//                  )
                  dut.output.insts_vec(i).inst.expect(ref_inst_list.head)
                  dut.output.insts_vec(i).pc.expect(ref_pc_list.head)
                  ref_inst_list = ref_inst_list.drop(1)
                  ref_pc_list = ref_pc_list.drop(1)
                }
              }

              dut.clock.step()
            }
          }
      }
    }
  }
}
