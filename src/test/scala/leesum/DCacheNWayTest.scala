package leesum

import chisel3._
import chiseltest._
import chiseltest.simulator.WriteFstAnnotation
import leesum.Cache.{DCacheNWay, DCacheReqType}
import org.scalatest.freespec.AnyFreeSpec

class DCacheNWayTest extends AnyFreeSpec with ChiselScalatestTester {

  "DCacheNWayTest1" in {
    test(new DCacheNWay(2)).withAnnotations(
      Seq(VerilatorBackendAnnotation, WriteFstAnnotation)
    ) { dut =>
      // --------------------
      // init port
      // --------------------
      dut.io.req_valid.poke(false.B)
      dut.io.clear_en.poke(false.B)

      dut.clock.step(2)

      // load lookup 0x80000000
      timescope {
        dut.io.req_valid.poke(true.B)
        dut.io.req_type.poke(DCacheReqType.load_lookup)
        dut.io.req_addr.poke(0x80000000L.U)
        dut.clock.step(1)
        dut.io.lookup_hit_way.valid.expect(false.B)
      }

      dut.clock.step(5)

      // store lookup 0x80000000
      timescope {
        dut.io.req_valid.poke(true.B)
        dut.io.req_type.poke(DCacheReqType.store_lookup)
        dut.io.req_addr.poke(0x80000000L.U)
        dut.clock.step(1)
        dut.io.lookup_hit_way.valid.expect(false.B)

      }

      dut.clock.step(5)

      // refill 0x80000000
      timescope {
        dut.io.req_valid.poke(true.B)
        dut.io.req_type.poke(DCacheReqType.refill)
        dut.io.req_addr.poke(0x80000000L.U)
        dut.io.req_way.poke(1.U)
        dut.io.write_data.poke(0x12345678L.U)
        dut.io.write_mask.poke(0xff.U)
        dut.clock.step(1)
      }

      // read 0x80000000
      timescope {
        dut.io.req_valid.poke(true.B)
        dut.io.req_type.poke(DCacheReqType.read)
        dut.io.req_addr.poke(0x80000000L.U)
        dut.io.req_way.poke(1.U)
        dut.clock.step(1)
        dut.io.read_data.data.expect(0x12345678L.U)
        dut.io.read_data.valid.expect(true.B)
        dut.io.read_data.dirty.expect(false.B)
        dut.io.read_data.tag.expect(0x200000.U)
      }

      dut.clock.step(1)
      // load lookup 0x80000000
      timescope {
        dut.io.req_valid.poke(true.B)
        dut.io.req_type.poke(DCacheReqType.load_lookup)
        dut.io.req_addr.poke(0x80000000L.U)
        dut.clock.step(1)
        dut.io.lookup_hit_way.valid.expect(true.B)
        dut.io.lookup_hit_way.bits.expect(1.U)
        dut.io.lookup_hit_data.expect(0x12345678L.U)
      }
      // load lookup 0x40000000
      timescope {
        dut.io.req_valid.poke(true.B)
        dut.io.req_type.poke(DCacheReqType.load_lookup)
        dut.io.req_addr.poke(0x40000000L.U)
        dut.clock.step(1)
        dut.io.lookup_hit_way.valid.expect(false.B)
      }

      // store lookup 0x40000000
      timescope {
        dut.io.req_valid.poke(true.B)
        dut.io.req_type.poke(DCacheReqType.store_lookup)
        dut.io.req_addr.poke(0x40000000L.U)
        dut.clock.step(1)
        dut.io.lookup_hit_way.valid.expect(false.B)
      }

      // store lookup 0x80000004
      timescope {
        dut.io.req_valid.poke(true.B)
        dut.io.req_type.poke(DCacheReqType.store_lookup)
        dut.io.req_addr.poke(0x80000004L.U)
        dut.clock.step(1)
        dut.io.lookup_hit_way.valid.expect(true.B)
        dut.io.lookup_hit_way.bits.expect(1.U)
        dut.io.lookup_hit_data.expect(0x12345678L.U)
      }

      // write  0x80000000
      timescope {
        dut.io.req_valid.poke(true.B)
        dut.io.req_type.poke(DCacheReqType.write)
        dut.io.req_addr.poke(0x80000000L.U)
        dut.io.req_way.poke(1.U)
        dut.io.write_data.poke(0xaabbccddL.U)
        dut.io.write_mask.poke(0xff.U)
        dut.clock.step(1)
      }

      // read 0x80000000
      timescope {
        dut.io.req_valid.poke(true.B)
        dut.io.req_type.poke(DCacheReqType.read)
        dut.io.req_addr.poke(0x80000000L.U)
        dut.io.req_way.poke(1.U)
        dut.clock.step(1)
        dut.io.read_data.data.expect(0xaabbccddL.U)
        dut.io.read_data.valid.expect(true.B)
        dut.io.read_data.dirty.expect(true.B)
        dut.io.read_data.tag.expect(0x200000.U)
      }
      dut.clock.step(5)
    }
  }
}
