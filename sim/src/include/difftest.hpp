#pragma once

#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>
#include <vector>
#include <ranges>
#include <format>
#include "CSREncode.h"
#include "Utils.h"
#include "Vtop.h"

struct Rv64emuBridge {
    void *sim;
};

extern "C" {

Rv64emuBridge *create_rv64emu(const char *isa,
                              const char *mmu_type,
                              uint64_t boot_pc,
                              uintptr_t memory_size,
                              uintptr_t memory_base,
                              uintptr_t hartid,
                              bool smode_enable,
                              bool umode_enable);

void destroy_rv64emu(Rv64emuBridge *rv64emu);

void load_file(Rv64emuBridge *rv64emu, const char *file_name);

void step(Rv64emuBridge *rv64emu, uint64_t steps);

uint64_t get_pc(Rv64emuBridge *rv64emu);

void set_pc(Rv64emuBridge *rv64emu, uint64_t pc);

uint64_t get_reg(Rv64emuBridge *rv64emu, uintptr_t idx);

void set_reg(Rv64emuBridge *rv64emu, uintptr_t idx, uint64_t val);

uint64_t get_csr(Rv64emuBridge *rv64emu, uint64_t addr);

void set_csr(Rv64emuBridge *rv64emu, uint64_t addr, uint64_t val);

} // extern "C"


class DiffTest {
private:


    static std::string_view get_csr_name(uint64_t addr) {
        MY_ASSERT((addr < 4096), "csr index out of range");
        switch (addr) {
            case MSTATUS:
                return "mstatus";
            case MCAUSE:
                return "mcause";
            case MTVAL:
                return "mtval";
            case MIP:
                return "mip";
            case MIE:
                return "mie";
            case MIDELEG:
                return "mideleg";
            case MEDELEG:
                return "medeleg";
            case MSCRATCH:
                return "mscratch";
            case MEPC:
                return "mepc";
            default:
                return "unknown";
        }
    }


    Rv64emuBridge *rv64emu_ref;
public:
    DiffTest(uint64_t boot_pc,
             uintptr_t memory_size,
             uintptr_t memory_base);

    void load_file(const char *file_name) {
        ::load_file(rv64emu_ref, file_name);
    }

    void step(uint64_t steps) {
        ::step(rv64emu_ref, steps);
    }

    uint64_t get_pc() {
        return ::get_pc(rv64emu_ref);
    }

    void set_pc(uint64_t pc) {
        ::set_pc(rv64emu_ref, pc);
    }

    uint64_t get_reg(uintptr_t idx) {
        return ::get_reg(rv64emu_ref, idx);
    }

    void set_reg(uintptr_t idx, uint64_t val) {
        ::set_reg(rv64emu_ref, idx, val);
    }

    uint64_t get_csr(uint64_t idx) {
        MY_ASSERT((idx < 4096), "csr index out of range");
        return ::get_csr(rv64emu_ref, idx);
    }

    void set_csr(uint64_t idx, uint64_t val) {
        ::set_csr(rv64emu_ref, idx, val);
    }

    using read_gpr_fuc = std::function<uint64_t(size_t idx)>;
    using read_csr_fuc = std::function<uint64_t(size_t idx)>;

    bool check_gprs(const read_gpr_fuc& dut_gpr, read_gpr_fuc ref_gpr, bool debug_en);

    bool check_csrs(const read_csr_fuc& dut_csr, bool debug_en);

    bool check_pc(uint64_t ref_pc, uint64_t dut_pc, bool debug_en);

    void ref_skip(const read_gpr_fuc& dut_gpr, uint64_t dut_pc);

    ~DiffTest();

};

DiffTest::DiffTest(uint64_t boot_pc,
                   uintptr_t memory_size,
                   uintptr_t memory_base) {
    rv64emu_ref = create_rv64emu("rv64imc", "bare", boot_pc, memory_size, memory_base, 0, false,true);
}

DiffTest::~DiffTest() {
    destroy_rv64emu(rv64emu_ref);
}

bool DiffTest::check_gprs(const DiffTest::read_gpr_fuc& dut_gpr, const DiffTest::read_gpr_fuc ref_gpr,
                          bool debug_en = false) {
    const auto reg_names = std::array{
            "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
            "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
            "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
            "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
    };

    bool fail = false;
    // first item is the register index, second item is the value
    auto no_pass_vec = std::vector<std::tuple<uint8_t, uint64_t>>();

    for (int idx: std::views::iota(0, 32)) {
        auto ref_val = ref_gpr(idx);
        auto dut_val = dut_gpr(idx);
        if (ref_val != dut_val) {
            no_pass_vec.emplace_back(idx, dut_val);
            fail = true;
        }
    }

    if (debug_en) {
        std::cout << "ref reg\n";
        for (int idx: std::views::iota(0, 8)) {
            std::cout << std::format("x{}: 0x{:016x} x{}: 0x{:016x} x{}: 0x{:016x} x{}: 0x{:016x}\n",
                                     idx * 4, ref_gpr(idx * 4),
                                     idx * 4 + 1, ref_gpr(idx * 4 + 1),
                                     idx * 4 + 2, ref_gpr(idx * 4 + 2),
                                     idx * 4 + 3, ref_gpr(idx * 4 + 3));
        }
        std::cout << "dut reg\n";
        for (int idx: std::views::iota(0, 8)) {
            std::cout << std::format("x{}: 0x{:016x} x{}: 0x{:016x} x{}: 0x{:016x} x{}: 0x{:016x}\n",
                                     idx * 4, dut_gpr(idx * 4),
                                     idx * 4 + 1, dut_gpr(idx * 4 + 1),
                                     idx * 4 + 2, dut_gpr(idx * 4 + 2),
                                     idx * 4 + 3, dut_gpr(idx * 4 + 3));
        }
    }

    for (const auto &item: no_pass_vec) {
        auto idx = std::get<0>(item);
        auto dut_val = std::get<1>(item);
        std::cout << std::format("reg {}({}) mismatch: ref: 0x{:016x}, dut: 0x{:016x}", idx, reg_names[idx],
                                 ref_gpr(idx), dut_val) << std::endl;
    }

    return fail;
}

bool DiffTest::check_pc(uint64_t ref_pc, uint64_t dut_pc, bool debug_en = false) {
    bool fail = false;

    if (debug_en) {
        std::cout << std::format("ref pc: 0x{:016x}, dut pc: 0x{:016x}\n", ref_pc, dut_pc);
    }

    if (ref_pc != dut_pc) {
        std::cout << std::format("pc mismatch: ref: 0x{:016x}, dut: 0x{:016x}\n", ref_pc, dut_pc);
        fail = true;
    }
    return fail;
}


bool DiffTest::check_csrs(const DiffTest::read_csr_fuc& dut_csr, bool debug_en = false) {
    const auto need_csr = std::array{MCAUSE, MEPC, MTVEC, MSTATUS, MIE, MTVAL};
    auto ref_csrs = std::vector<uint64_t>();
    auto dut_csrs = std::vector<uint64_t>();


    for (auto csr_addr: need_csr) {
        ref_csrs.emplace_back(get_csr(csr_addr));
        dut_csrs.emplace_back(dut_csr(csr_addr));
    }

    bool mismatch = false;
    for (auto idx = 0; idx < need_csr.size(); idx++) {
        if (dut_csrs[idx] != ref_csrs[idx]) {
            mismatch = true;
            std::cout << std::format("csr {:x},{:s} mismatch: ref: 0x{:016x}, dut: 0x{:016x}\n", need_csr[idx],
                                     get_csr_name(need_csr[idx]), ref_csrs[idx],
                                     dut_csrs[idx]);
        }
    }

    return mismatch;
}

void DiffTest::ref_skip(const DiffTest::read_gpr_fuc& dut_gpr, uint64_t dut_pc) {
    set_pc(dut_pc);
    for (int idx: std::views::iota(0, 32)) {
        set_reg(idx, dut_gpr(idx));
    }
}


