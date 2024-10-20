package leesum.dbg

import chisel3._
import chisel3.util.Cat
import leesum.{BitMaskHelper, CatReverse}

//#[bitfield(u32)]
//pub struct DMStatus {
//  #[bits(4)]
//  pub version: u8,
//  pub confstrptrvalid: bool,
//  pub hasresethaltreq: bool,
//  pub authbusy: bool,
//  pub authenticated: bool,
//  pub anyhalted: bool,
//  pub allhalted: bool,
//  pub anyrunning: bool,
//  pub allrunning: bool,
//  pub anyunavail: bool,
//  pub allunavail: bool,
//  pub anynonexistent: bool,
//  pub allnonexistent: bool,
//  pub anyresumeack: bool,
//  pub allresumeack: bool,
//  pub anyhavereset: bool,
//  pub allhavereset: bool,
//  #[bits(2)]
//  pub zero0: u8,
//  pub impebreak: bool,
//  pub stickyunavail: bool,
//  pub ndmresetpending: bool,
//  #[bits(7)]
//  pub zero1: u8,
//}

class DMStatusFiled(data: UInt) {
  require(data.getWidth == 32)
  def version: UInt = data(3, 0)
  def confstrptrvalid: Bool = data(4)
  def hasresethaltreq: Bool = data(5)
  def authbusy: Bool = data(6)
  def authenticated: Bool = data(7)
  def anyhalted: Bool = data(8)
  def allhalted: Bool = data(9)
  def anyrunning: Bool = data(10)
  def allrunning: Bool = data(11)
  def anyunavail: Bool = data(12)
  def allunavail: Bool = data(13)
  def anynonexistent: Bool = data(14)
  def allnonexistent: Bool = data(15)
  def anyresumeack: Bool = data(16)
  def allresumeack: Bool = data(17)
  def anyhavereset: Bool = data(18)
  def allhavereset: Bool = data(19)
  def zero0: UInt = data(21, 20)
  def impebreak: Bool = data(22)
  def stickyunavail: Bool = data(23)
  def ndmresetpending: Bool = data(24)
  def zero1: UInt = data(31, 25)

  def raw: UInt = data

  def get_read_data(debug_state: DbgSlaveState): UInt = {
    val r_dmstatus = CatReverse(
      version,
      confstrptrvalid,
      hasresethaltreq,
      authbusy,
      authenticated,
      debug_state.halted(), // anyhalted,
      debug_state.halted(), // allhalted,
      debug_state.running(), // anyrunning,
      debug_state.running(),
      false.B, // anyunavail,
      false.B, // allunavail,
      false.B, // anynonexistent,
      false.B, // allnonexistent,
      debug_state.resumeack(), // ,  anyresumeack,
      debug_state.resumeack(), // allresumeack,
      debug_state.have_reset(), // anyhavereset,
      debug_state.have_reset(), // allhavereset,
      zero0,
      impebreak,
      false.B, // TODO:  check implement stickyunavail
      ndmresetpending,
      zero1
    )
    require(r_dmstatus.getWidth == 32)
    r_dmstatus
  }
}

object DMStatusMask extends BitMaskHelper {
  def version = gen_mask(3, 0)
  def confstrptrvalid = gen_mask(4)
  def hasresethaltreq = gen_mask(5)
  def authbusy = gen_mask(6)
  def authenticated = gen_mask(7)
  def anyhalted = gen_mask(8)
  def allhalted = gen_mask(9)
  def anyrunning = gen_mask(10)
  def allrunning = gen_mask(11)
  def anyunavail = gen_mask(12)
  def allunavail = gen_mask(13)
  def anynonexistent = gen_mask(14)
  def allnonexistent = gen_mask(15)
  def anyresumeack = gen_mask(16)
  def allresumeack = gen_mask(17)
  def anyhavereset = gen_mask(18)
  def allhavereset = gen_mask(19)
  def zero0 = gen_mask(21, 20)
  def impebreak = gen_mask(22)
  def stickyunavail = gen_mask(23)
  def ndmresetpending = gen_mask(24)
  def zero1 = gen_mask(31, 25)

}

//#[bitfield(u32)]
//pub struct DMControl {
//  pub dmactive: bool,
//  pub ndmreset: bool,
//  pub clrresethaltreq: bool,
//  pub setresethaltreq: bool,
//  pub clrkeepalive: bool,
//  pub setkeepalive: bool,
//  #[bits(10)]
//  pub hartselhi: u16,
//  #[bits(10)]
//  pub hartsello: u16,
//  pub hasel: bool,
//  pub ackunavail: bool,
//  pub ackhavereset: bool,
//  pub hartreset: bool,
//  pub resumereq: bool,
//  pub haltreq: bool,
//}

class DMControlFiled(data: UInt) {
  require(data.getWidth == 32)
  def dmactive: Bool = data(0)
  def ndmreset: Bool = data(1)
  def clrresethaltreq: Bool = data(2)
  def setresethaltreq: Bool = data(3)
  def clrkeepalive: Bool = data(4)
  def setkeepalive: Bool = data(5)
  def hartselhi: UInt = data(15, 6)
  def hartsello: UInt = data(25, 16)
  def hasel: Bool = data(26)
  def ackunavail: Bool = data(27)
  def ackhavereset: Bool = data(28)
  def hartreset: Bool = data(29)
  def resumereq: Bool = data(30)
  def haltreq: Bool = data(31)

  def raw: UInt = data

//  let mut r_dmcontrol = self.dmcontrol;
//
//  // r_dmcontrol.set_resumereq(self.hart_debug_state.resumeack);
//
//  let mut hart0 = self.hart0.borrow_mut(); // only support single hart now
//
//  r_dmcontrol.set_hartreset(hart0.havereset());
//  // only support single hart now
//  r_dmcontrol.set_hasel(false);
//  r_dmcontrol.set_hartselhi(0);
//  r_dmcontrol.set_hartsello(0);
//
//  r_dmcontrol.set_dmactive(self.dmcontrol.dmactive());
//  r_dmcontrol.set_ndmreset(self.dmcontrol.ndmreset());

  def get_read_data(new_havereset: Bool): UInt = {
    val r_dmcontrol = Cat(
      0.U(2.W),
      new_havereset,
      0.U(27.W),
      ndmreset,
      dmactive
    )
    require(r_dmcontrol.getWidth == 32)
    r_dmcontrol
  }

}

//#[bitfield(u32)]
//pub struct HartInfo {
//  #[bits(12)]
//  pub dataaddr: u16,
//  #[bits(4)]
//  pub datasize: u8,
//  pub dataaccess: bool,
//  #[bits(3)]
//  pub zero0: u8,
//  #[bits(4)]
//  pub nscratch: u8,
//  pub zero1: u8,
//}

class HartInfoFiled(data: UInt) {
  require(data.getWidth == 32)
  def dataaddr: UInt = data(11, 0)
  def datasize: UInt = data(15, 12)
  def dataaccess: Bool = data(16)
  def zero0: UInt = data(19, 17)
  def nscratch: UInt = data(23, 20)
  def zero1: UInt = data(31, 24)

  def raw: UInt = data
}
//#[bitfield(u32)]
//pub struct HaWindowSel {
//  #[bits(15)]
//  hawindowsel: u16,
//  #[bits(17)]
//  zero0: u32,
//}
class HaWindowSelFiled(data: UInt) {
  require(data.getWidth == 32)
  def hawindowsel: UInt = data(14, 0)
  def zero0: UInt = data(31, 15)

  def raw: UInt = data
}

//#[bitfield(u32)]
//pub struct Abstractcs {
//  #[bits(4)]
//  pub datacount: u8,
//  #[bits(4)]
//  pub zero0: u8,
//  #[bits(3)]
//  pub cmderr: u8,
//  pub relaxedpriv: bool,
//  pub busy: bool,
//  #[bits(11)]
//  pub zero1: u16,
//  #[bits(5)]
//  pub progbufsize: u8,
//  #[bits(3)]
//  pub zero2: u8,
//}

class AbstractcsFiled(data: UInt) {
  require(data.getWidth == 32)
  def datacount: UInt = data(3, 0)
  def zero0: UInt = data(7, 4)
  def cmderr: UInt = data(10, 8)
  def relaxedpriv: Bool = data(11)
  def busy: Bool = data(12)
  def zero1: UInt = data(23, 13)
  def progbufsize: UInt = data(28, 24)
  def zero2: UInt = data(31, 29)

  def raw: UInt = data

  def get_new_write_data(wdata: UInt): UInt = {
    val w_cmderr = wdata(10, 8)
    val new_cmderr = Wire(UInt(3.W))

    when(busy) {
      // Writing this register while an abstract command is executing causes cmderr to become 1 (busy) once
      new_cmderr := DbgPKG.CMDERR_BUSY.U
    }.otherwise {
      // this field remain set until they are cleared by writ-
      // ing 1 to them. No abstract command is started
      // until the value is reset to 0.
      new_cmderr := cmderr & !w_cmderr
    }

    val w_abstractcs = CatReverse(
      datacount,
      zero0,
      new_cmderr,
      relaxedpriv,
      busy,
      zero1,
      progbufsize,
      zero2
    )
    require(w_abstractcs.getWidth == 32)
    w_abstractcs
  }

  def set_busy_and_cmderr_value(cmderr_value: UInt, busy_value: Bool): UInt = {
    require(cmderr_value.getWidth == 3)
    val w_abstractcs = CatReverse(
      datacount,
      zero0,
      cmderr_value,
      relaxedpriv,
      busy_value,
      zero1,
      progbufsize,
      zero2
    )
    require(w_abstractcs.getWidth == 32)
    w_abstractcs
  }
}

object AbstractcsMask extends BitMaskHelper {
  def datacount: Long = gen_mask(3, 0)
  def zero0: Long = gen_mask(7, 4)
  def cmderr: Long = gen_mask(10, 8)
  def relaxedpriv: Long = gen_mask(11)
  def busy: Long = gen_mask(12)
  def zero1: Long = gen_mask(23, 13)
  def progbufsize: Long = gen_mask(28, 24)
  def zero2: Long = gen_mask(31, 29)
}

class CommandRegFiled(data: UInt) {
  require(data.getWidth == 32)
  def regno: UInt = data(15, 0)
  def write: Bool = data(16)
  def transfer: Bool = data(17)
  def postexec: Bool = data(18)
  def aarpostincrement: Bool = data(19)
  def aarsize: UInt = data(22, 20)
  def zero0: Bool = data(23)
  def cmdtype: UInt = data(31, 24)

  def raw: UInt = data

  def is_csr: Bool = {
    regno(15, 12) === 0.U
  }

  def is_gpr: Bool = {
    regno(15, 12) === 1.U && regno(11, 6) === 0.U
  }

  def get_gpr_regno: UInt = {
    regno(5, 0)
  }

  def get_csr_regno: UInt = {
    regno(11, 0)
  }

}

object CommandRegMask {

//  require(data.getWidth == 32)
//  def regno: UInt = data(15, 0)
//  def write: Bool = data(16)
//  def transfer: Bool = data(17)
//  def postexec: Bool = data(18)
//  def aarpostincrement: Bool = data(19)
//  def aarsize: UInt = data(22, 20)
//  def zero0: Bool = data(23)
//  def cmdtype: UInt = data(31, 24)
//
//  def raw: UInt = data

  def regno: Int = 0x0000_ffff
  def write: Int = 0x0001_0000
  def transfer: Int = 0x0002_0000
  def postexec: Int = 0x0004_0000
  def aarpostincrement: Int = 0x0008_0000
  def aarsize: Int = 0x0070_0000
  def zero0: Int = 0x0080_0000
  def cmdtype: Int = 0xff00_0000

  def all: Int = 0xffff_ffff
}

//#[bitfield(u32)]
//pub struct CommandMem {
//  #[bits(14)]
//  pub zero0: u16,
//  #[bits(2)]
//  pub target_specific: u8,
//  pub write: bool,
//  #[bits(2)]
//  pub zero1: u8,
//  pub aampostincrement: bool,
//  #[bits(3)]
//  pub aamsize: u8,
//  pub aamvirtual: bool,
//  pub cmdtype: u8,
//}

class CommandMemFiled(data: UInt) {
  require(data.getWidth == 32)
  def zero0: UInt = data(13, 0)
  def target_specific: UInt = data(15, 14)
  def write: Bool = data(16)
  def zero1: UInt = data(18, 17)
  def aampostincrement: Bool = data(19)
  def aamsize: UInt = data(22, 20)
  def aamvirtual: Bool = data(23)
  def cmdtype: UInt = data(31, 24)

  def raw: UInt = data
}

object CommandMemMask {
  def zero0: Int = 0x0000_3fff
  def target_specific: Int = 0x0000_c000
  def write: Int = 0x0001_0000
  def zero1: Int = 0x0006_0000
  def aampostincrement: Int = 0x0008_0000
  def aamsize: Int = 0x0070_0000
  def aamvirtual: Int = 0x0080_0000
  def cmdtype: Int = 0xff00_0000

  def all: Int = 0xffff_ffff

}

//#[bitfield(u32)]
//pub struct Command {
//  #[bits(24)]
//  pub control: u32,
//  pub cmdtype: u8,
//}

class CommandFiled(data: UInt) {
  require(data.getWidth == 32)
  def control: UInt = data(23, 0)
  def cmdtype: UInt = data(31, 24)

  def raw: UInt = data

  def reg_field: CommandRegFiled = {
    new CommandRegFiled(data)
  }
  def mem_field: CommandMemFiled = {
    new CommandMemFiled(data)
  }
}

//#[bitfield(u32)]
//pub struct CommandReg {
//  pub regno: u16,
//  pub write: bool,
//  pub transfer: bool,
//  pub postexec: bool,
//  pub aarpostincrement: bool,
//  #[bits(3)]
//  pub aarsize: u8,
//  pub zero0: bool,
//  pub cmdtype: u8,
//}

//#[bitfield(u32)]
//pub struct AbstractAuto {
//  #[bits(12)]
//  autoexecdata: u16,
//  #[bits(4)]
//  zero0: u8,
//  autoexecprogbuf: u16,
//}

class AbstractAutoFiled(data: UInt) {
  require(data.getWidth == 32)
  def autoexecdata: UInt = data(11, 0)
  def zero0: UInt = data(15, 12)
  def autoexecprogbuf: UInt = data(31, 16)

  def raw: UInt = data
}

//#[bitfield(u32)]
//pub struct DMCs2 {
//  hgselect: bool,
//  hgwrite: bool,
//  #[bits(5)]
//  group: u8,
//  #[bits(4)]
//  dmexttrigger: u8,
//  grouptype: bool,
//  #[bits(20)]
//  zero0: u32,
//}

class DMCs2Filed(data: UInt) {
  require(data.getWidth == 32)
  def hgselect: Bool = data(0)
  def hgwrite: Bool = data(1)
  def group: UInt = data(6, 2)
  def dmexttrigger: UInt = data(10, 7)
  def grouptype: Bool = data(11)
  def zero0: UInt = data(31, 12)

  def raw: UInt = data
}

//#[bitfield(u32)]
//pub struct SBCS {
//  pub sbaccess8: bool,
//  pub sbaccess16: bool,
//  pub sbaccess32: bool,
//  pub sbaccess64: bool,
//  pub sbaccess128: bool,
//  #[bits(7)]
//  pub sbasize: u8,
//  #[bits(3)]
//  pub sberror: u8,
//  pub sbreadondata: bool,
//  pub sbautoincrement: bool,
//  #[bits(3)]
//  pub sbaccess: u8,
//  pub sbreadonaddr: bool,
//  pub sbbusy: bool,
//  pub sbbusyerror: bool,
//  #[bits(6)]
//  pub zero0: u8,
//  #[bits(3)]
//  pub sbversion: u8,
//}

class SBCSFiled(data: UInt) {
  require(data.getWidth == 32)
  def sbaccess8: Bool = data(0)
  def sbaccess16: Bool = data(1)
  def sbaccess32: Bool = data(2)
  def sbaccess64: Bool = data(3)
  def sbaccess128: Bool = data(4)
  def sbasize: UInt = data(11, 5)
  def sberror: UInt = data(14, 12)
  def sbreadondata: Bool = data(15)
  def sbautoincrement: Bool = data(16)
  def sbaccess: UInt = data(19, 17)
  def sbreadonaddr: Bool = data(20)
  def sbbusy: Bool = data(21)
  def sbbusyerror: Bool = data(22)
  def zero0: UInt = data(28, 23)
  def sbversion: UInt = data(31, 29)

  def raw: UInt = data
}

//#[bitfield(u32)]
//struct DTMIDCode {
//  _pad: bool,
//  #[bits(11)]
//  manufld: u16,
//  #[bits(16)]
//  part_number: u16,
//  #[bits(4)]
//  version: u8,
//}

class DTMIDCodeFiled(data: UInt) {
  require(data.getWidth == 32)

  def _pad = data(0)
  def manufld = data(11, 1)
  def part_number = data(27, 12)
  def version = data(31, 28)

  def raw: UInt = data
}

//#[bitfield(u32)]
//struct DTMCS {
//  #[bits(4)]
//  version: u8,
//  #[bits(6)]
//  abits: u8,
//  #[bits(2)]
//  dmistat: u8,
//  #[bits(3)]
//  idle: u8,
//  _pad: bool,
//  dmireset: bool,
//  dtmhardreset: bool,
//  #[bits(3)]
//  errinfo: u8,
//  #[bits(11)]
//  _pad: u16,
//}

class DTMDTMCSFiled(data: UInt) {
  require(data.getWidth == 32)
  def version = data(3, 0)
  def abits = data(9, 4)
  def dmistat = data(11, 10)
  def idle = data(14, 12)
  def _pad0 = data(15)
  def dmireset = data(16)
  def dmihardreset = data(17)
  def _pad1 = data(31, 18)

  def raw: UInt = data

  def get_read_data(new_dmistat: UInt): UInt = {
    require(new_dmistat.getWidth == 2)
    val r_dtmcs = CatReverse(
      version,
      abits,
      new_dmistat,
      idle,
      _pad0,
      false.B, // dmireset,
      false.B, // dmihardreset,
      0.U(14.W)
    )
    require(r_dtmcs.getWidth == 32)
    r_dtmcs
  }
}

object DTMCSMask {
  def version = 0x0000_000f
  def abits = 0x0000_03f0
  def dmistat = 0x0000_0c00
  def idle = 0x0000_7000
  def _pad0 = 0x0000_8000
  def dmireset = 0x0001_0000
  def dmihardreset = 0x0002_0000

  def all = 0xffff_ffff
}

//#[bitfield(u64)]
//struct DTMI {
//  #[bits(2)]
//  op: u8,
//  data: u32,
//  #[bits(30)]
//  address: u32,
//}

class DTMDMIFiled(data_reg: UInt) {
  def op = data_reg(1, 0)
  def data = data_reg(33, 2)
  def address = data_reg(data_reg.getWidth - 1, 34)

  def raw: UInt = data_reg

  def get_read_data(new_op: UInt): UInt = {
    require(new_op.getWidth == 2)
    val r_dmi = CatReverse(
      new_op,
      data,
      address
    )
    r_dmi
  }
}

object DTMDMIMask extends BitMaskHelper {
  def op = gen_mask(1, 0)
  def data = gen_mask(33, 2)
  def addr = gen_mask(63, 34)
}

object DbgPKG {
//  pub const ABSTRACT_DATA_BASE: usize = 0x04;
//  pub const DMCONTROL_ADDR: usize = 0x10;
//  pub const DMSTATUS_ADDR: usize = 0x11;
//  pub const HARTINFO_ADDR: usize = 0x12;
//  pub const HALTSUM1_ADDR: usize = 0x13;
//  pub const HAWINDOWSEL_ADDR: usize = 0x14;
//  pub const HAWINDOW: usize = 0x15;
//  pub const ABSTRACTCS_ADDR: usize = 0x16;
//  pub const COMMAND_ADDR: usize = 0x17;
//  pub const ABSTRACTAUTO_ADDR: usize = 0x18;
//  pub const PROGBUF_BASE: usize = 0x20;

  val ABSTRACT_DATA_BASE = 0x04
  val DMCONTROL_ADDR = 0x10
  val DMSTATUS_ADDR = 0x11
  val HARTINFO_ADDR = 0x12
  val HALTSUM1_ADDR = 0x13
  val HAWINDOWSEL_ADDR = 0x14
  val HAWINDOW = 0x15
  val ABSTRACTCS_ADDR = 0x16
  val COMMAND_ADDR = 0x17
  val ABSTRACTAUTO_ADDR = 0x18
  val PROGBUF_BASE = 0x20

  val SBCS_ADDR = 0x38

//  pub const DMSTATUS_VERSION_NONE: usize = 0;
//  pub const DMSTATUS_VERSION0_11: usize = 1;
//  pub const DMSTATUS_VERSION0_13: usize = 2;
//  pub const DMSTATUS_VERSION1_0: usize = 3;
//
//  // Command types
//  pub const COMDTYPE_ACCESS_REG: usize = 0;
//  pub const COMDTYPE_QUICK_ACCESS: usize = 1;
//  pub const COMDTYPE_ACCESS_MEM: usize = 2;
//
//  // aarsize
//  pub const AARSIZE_32: usize = 2;
//  pub const AARSIZE_64: usize = 3;
//  pub const AARSIZE_128: usize = 4;
//
//  // aamsize
//  pub const AAMSIZE_8: usize = 0;
//  pub const AAMSIZE_16: usize = 1;
//  pub const AAMSIZE_32: usize = 2;
//  pub const AAMSIZE_64: usize = 3;
//  pub const AAMSIZE_128: usize = 4;
//
//  // cmderr
//  pub const CMDERR_NONE: usize = 0;
//  pub const CMDERR_BUSY: usize = 1;
//  pub const CMDERR_NOTSUP: usize = 2;
//  pub const CMDERR_EXCEPTION: usize = 3;
//  pub const CMDERR_HALT_RESUME: usize = 4;
//  pub const CMDERR_BUS: usize = 5;
//  pub const CMDERR_RESERVED: usize = 6;
//  pub const CMDERR_OTHER: usize = 7;
//
//  // sbaccess
//  pub const SBACCESS_8: usize = 0;
//  pub const SBACCESS_16: usize = 1;
//  pub const SBACCESS_32: usize = 2;
//  pub const SBACCESS_64: usize = 3;
//  pub const SBACCESS_128: usize = 4;
//
//  // sberror
//  pub const SBERROR_NONE: usize = 0;
//  pub const SBERROR_TIMEOUT: usize = 1;
//  pub const SBERROR_BADADDR: usize = 2;
//  pub const SBERROR_ALIGNMENT: usize = 3;
//  pub const SBERROR_SIZE: usize = 4;
//  pub const SBERROR_OTHER: usize = 7;

  val DMSTATUS_VERSION_NONE = 0
  val DMSTATUS_VERSION0_11 = 1
  val DMSTATUS_VERSION0_13 = 2
  val DMSTATUS_VERSION1_0 = 3

  val COMDTYPE_ACCESS_REG = 0
  val COMDTYPE_QUICK_ACCESS = 1
  val COMDTYPE_ACCESS_MEM = 2

  val AARSIZE_32 = 2
  val AARSIZE_64 = 3
  val AARSIZE_128 = 4

  val AAMSIZE_8 = 0
  val AAMSIZE_16 = 1
  val AAMSIZE_32 = 2
  val AAMSIZE_64 = 3
  val AAMSIZE_128 = 4

  val CMDERR_NONE = 0
  val CMDERR_BUSY = 1
  val CMDERR_NOTSUP = 2
  val CMDERR_EXCEPTION = 3
  val CMDERR_HALT_RESUME = 4
  val CMDERR_BUS = 5
  val CMDERR_RESERVED = 6
  val CMDERR_OTHER = 7

  val SBACCESS_8 = 0
  val SBACCESS_16 = 1
  val SBACCESS_32 = 2
  val SBACCESS_64 = 3
  val SBACCESS_128 = 4

  val SBERROR_NONE = 0
  val SBERROR_TIMEOUT = 1
  val SBERROR_BADADDR = 2
  val SBERROR_ALIGNMENT = 3
  val SBERROR_SIZE = 4
  val SBERROR_OTHER = 7

// const DMI_OP_STATUS_SUCCESS: u8 = 0;
// const DMI_OP_STATUS_RESERVED: u8 = 1;
// const DMI_OP_STATUS_FAILED: u8 = 2;
// const DMI_OP_STATUS_BUSY: u8 = 3;

// const DMI_OP_NOP: u8 = 0;
// const DMI_OP_READ: u8 = 1;
// const DMI_OP_WRITE: u8 = 2;
// const DMI_OP_RESERVED: u8 = 3;

  val DMI_OP_STATUS_SUCCESS = 0
  val DMI_OP_STATUS_RESERVED = 1
  val DMI_OP_STATUS_FAILED = 2
  val DMI_OP_STATUS_BUSY = 3

  val DMI_OP_NOP = 0
  val DMI_OP_READ = 1
  val DMI_OP_WRITE = 2
  val DMI_OP_RESERVED = 3

//    typedef enum logic [IrLength-1:0] {
//      BYPASS0   = 'h0,
//      IDCODE    = 'h1,
//      DTMCSR    = 'h10,
//      DMIACCESS = 'h11,
//      BYPASS1   = 'h1f
//    } ir_reg_e;

  val JtagDTM_BYPASS0 = 0x0
  val JtagDTM_IDCODE = 0x1
  val JtagDTM_DTMCS = 0x10
  val JtagDTM_DMI = 0x11
  val JtagDTM_CUSTOM = 0x12
  val JtagDTM_BYPASS1 = 0x1f

}
