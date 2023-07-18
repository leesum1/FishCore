package leesum

import Chisel.Cat
import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import net.fornwall.jelf
import chiseltest.simulator.WriteVcdAnnotation

import scala.io.Source
import java.nio.file.{Files, Paths}
import java.lang
import java.math.BigInteger

class RspPacket2Insts_test extends AnyFreeSpec with ChiselScalatestTester {

  "test combinational circuits" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(8.W))
        val out = Output(UInt(8.W))
      })
      val reg = RegInit(0.U(8.W))
      reg := io.in


      val temp = (io.in + 1.U)  + reg
      io.out := temp
    }).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { c =>
      c.io.in.poke(1.U)
      c.io.out.expect(2.U)
      c.clock.step()

      c.io.in.poke(2.U)
      c.io.out.expect(4.U)
      c.clock.step()

      c.io.in.poke(3.U)
      c.io.out.expect(6.U)
      c.clock.step()

      c.io.in.poke(4.U)
      c.io.out.expect(8.U)
      c.clock.step()

      c.io.in.poke(5.U)
      c.io.out.expect(10.U)
      c.clock.step()

    }
  }

  "RspPacket2Insts_test" in {
    test(new RspPacket2Insts).withAnnotations(
      Seq(IcarusBackendAnnotation, WriteVcdAnnotation)
    ) { dut =>
      {
        dut.input.initSource()
        dut.input.setSourceClock(dut.clock)

        /** **********准备数据 **************
          */
        val filename =
          "/home/leesum/workhome/riscv64-emu/ready_to_run/linux.elf"
        val content = Files.readAllBytes(Paths.get(filename))
        val elfFile = jelf.ElfFile.from(content)
// Find the section containing the function

        val symbol = elfFile.getELFSymbol("sbi_init");

        val entry_point = symbol.st_value;
        println(symbol.st_value.toHexString)

//        elfFile.e_phnum

        val seg = (0 until elfFile.e_phnum)
          .map(elfFile.getProgramHeader)
          .filter(_.p_type == jelf.ElfSegment.PT_LOAD)
          .filter(seg => {
            (entry_point >= seg.p_vaddr) && (entry_point < seg.p_vaddr + seg.p_memsz)
          })
          .head

        val offset = entry_point - seg.p_vaddr + seg.p_offset

        val func_data =
          content.slice(offset.toInt, offset.toInt + symbol.st_size.toInt)

        val inst_pak = func_data
          .grouped(2)
          .map(group => {
            val padded = group ++ Array.fill[Byte](2 - group.length)(0)

            new BigInteger(1, padded.reverse).intValue()
          })
          .toArray

        var idx = 0
        var last_valid = false
        var last_inst = 0

        var inst_list = List[UInt]()
        while (idx < inst_pak.size) {
          if (last_valid) {
            val inst = (inst_pak(idx) & 0xffff) << 16 | last_inst
            last_valid = false

//            println(inst)
            inst_list :+= ("x" + inst.toHexString).U

          } else {
            if ((inst_pak(idx) & 0x03) != 0x03) {
//              println(inst_pak(idx))

              inst_list :+= ("x" + inst_pak(idx).toHexString).U
            } else {
              last_valid = true
              last_inst = inst_pak(idx) & 0xffff
            }
          }
          idx += 1
        }
//        println("-------------------------------")
//        val insts = inst_list.foreach(x => {
//          println(x.toString())
//        })

//        assert(false)
        dut.clock.step(10)

        val longs = func_data
          .grouped(8)
          .map(group => {
            val padded = group ++ Array.fill[Byte](8 - group.length)(0)
            var hexstring =
              "x" + new BigInteger(1, padded.reverse).longValue().toHexString
            hexstring
          })
          .foreach(x => {

            println("hello")

            dut.input.valid.poke(true.B)
            dut.input.bits.paket_payload.poke(x.U)

            for (i <- 0 until dut.input.bits.insts_per_packet) {
              if (dut.output.insts_mask(i).peek().litToBoolean) {
                print(i + " dut:")
                println(dut.output.insts(i).peek().litValue.toLong.toHexString)
                print(i + " ref:")
                println(inst_list.head.litValue.toLong.toHexString)
                dut.output.insts(i).expect(inst_list.head)
                println("after peek")
                inst_list = inst_list.drop(1)
              }
            }

            dut.clock.step()

//            println(pe.insts(0).litValue.toLong.toHexString)
          })
      }
    }
  }
}
