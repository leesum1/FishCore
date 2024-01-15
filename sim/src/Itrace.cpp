//
// Created by leesum on 1/14/24.
//

#include "include/Itrace.h"


#include "capstone.h"


Itrace::Itrace() {
	logger = spdlog::get("console");
	itrace_log = spdlog::get("itrace");
	logger->info("Itrace init Finished");

	cs_err err = cs_open(CS_ARCH_RISCV, static_cast<cs_mode>(CS_MODE_RISCV64C | CS_MODE_RISCVC), &handle);
	if (err) {
		logger->critical("Failed on cs_open() with error returned: %u\n", err);
		exit(-1);
	}
}

Itrace::~Itrace() {
	cs_close(&handle);
	logger->info("Itrace exit");
}

void Itrace::riscv_disasm(uint32_t code, const uint64_t pc) {
	cs_insn* insn;
	uint8_t const* arr = reinterpret_cast<uint8_t*>(&code);

	// TODO: use cs_disasm_iter
	const auto count = cs_disasm(handle, arr, 4, pc, 1, &insn);

	if (count != 0) {
		itrace_log->info("0x{:x}:\t{}\t{}", insn[0].address, insn[0].mnemonic, insn[0].op_str);
	}
	cs_free(insn, count);
}




