package leesum.devices

import chisel3._

///* ref spike plic */
//const _PLIC_MAX_CONTEXTS: usize = 15872;
///*
// * The PLIC consists of memory-mapped control registers, with a memory map
// * as follows:
// *
// * base + 0x000000: Reserved (interrupt source 0 does not exist)
// * base + 0x000004: Interrupt source 1 priority
// * base + 0x000008: Interrupt source 2 priority
// * ...
// * base + 0x000FFC: Interrupt source 1023 priority
// * base + 0x001000: Pending 0
// * base + 0x001FFF: Pending
// * base + 0x002000: Enable bits for sources 0-31 on context 0
// * base + 0x002004: Enable bits for sources 32-63 on context 0
// * ...
// * base + 0x0020FC: Enable bits for sources 992-1023 on context 0
// * base + 0x002080: Enable bits for sources 0-31 on context 1
// * ...
// * base + 0x002100: Enable bits for sources 0-31 on context 2
// * ...
// * base + 0x1F1F80: Enable bits for sources 992-1023 on context 15871
// * base + 0x1F1F84: Reserved
// * ...		    (higher context IDs would fit here, but wouldn't fit
// *		     inside the per-context priority vector)
// * base + 0x1FFFFC: Reserved
// * base + 0x200000: Priority threshold for context 0
// * base + 0x200004: Claim/complete for context 0
// * base + 0x200008: Reserved
// * ...
// * base + 0x200FFC: Reserved
// * base + 0x201000: Priority threshold for context 1
// * base + 0x201004: Claim/complete for context 1
// * ...
// * base + 0xFFE000: Priority threshold for context 15871
// * base + 0xFFE004: Claim/complete for context 15871
// * base + 0xFFE008: Reserved
// * ...
// * base + 0xFFFFFC: Reserved
// */
//
///* Each interrupt source has a priority register associated with it. */
//const PRIORITY_BASE: u64 = 0;
//const _PRIORITY_PER_ID: u64 = 4;
//const PRIORITY_END: u64 = PENDING_BASE - 1;
//
///* Each interrupt source has a pending bit associated with it. */
//const PENDING_BASE: u64 = 0x1000;
//const PENDING_END: u64 = ENABLE_BASE - 1;
///*
// * Each hart context has a vector of interupt enable bits associated with it.
// * There's one bit for each interrupt source.
// */
//const ENABLE_BASE: u64 = 0x2000;
//const ENABLE_PER_HART: u64 = 0x80;
//const ENABLE_END: u64 = CONTEXT_BASE - 1;
///*
// * Each hart context has a set of control registers associated with it.  Right
// * now there's only two: a source priority threshold over which the hart will
// * take an interrupt, and a register to claim interrupts.
// */
//const CONTEXT_BASE: u64 = 0x200000;
//const CONTEXT_PER_HART: u64 = 0x1000;
//const CONTEXT_THRESHOLD: u64 = 0;
//const CONTEXT_CLAIM: u64 = 4;
object PlicConst {
  val priority_base = 0
  val priority_per_id = 4
  val priority_end = pending_base
  val pending_base = 0x1000
  val pending_end = enable_base
  val enable_base = 0x2000
  val enable_per_hart = 0x80
  val enable_end = context_base
  val context_base = 0x200000
  val context_per_hart = 0x1000
  val context_threshold = 0
  val context_claim = 4
}

class plic extends Module {}
