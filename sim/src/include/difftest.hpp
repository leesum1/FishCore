#pragma once

#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>
#include <vector>

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
                              bool smode_enable);

void destroy_rv64emu(Rv64emuBridge *rv64emu);

void load_file(Rv64emuBridge *rv64emu, const char *file_name);

void step(Rv64emuBridge *rv64emu, uint64_t steps);

uint64_t get_pc(Rv64emuBridge *rv64emu);

void set_pc(Rv64emuBridge *rv64emu, uint64_t pc);

uint64_t get_reg(Rv64emuBridge *rv64emu, uintptr_t idx);

void set_reg(Rv64emuBridge *rv64emu, uintptr_t idx, uint64_t val);

} // extern "C"


class DiffTest {
private:
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

    using read_gpr_fuc = std::function<uint64_t(size_t idx)>;

    bool check_gprs(read_gpr_fuc dut_gpr, read_gpr_fuc ref_gpr, bool debug_en);


    bool check_pc(uint64_t ref_pc, uint64_t dut_pc);

    ~DiffTest();

};

DiffTest::DiffTest(uint64_t boot_pc,
                   uintptr_t memory_size,
                   uintptr_t memory_base) {
    rv64emu_ref = create_rv64emu("rv64im", "bare", boot_pc, memory_size, memory_base, 0, false);
}

DiffTest::~DiffTest() {
    destroy_rv64emu(rv64emu_ref);
}

bool DiffTest::check_gprs(DiffTest::read_gpr_fuc dut_gpr, DiffTest::read_gpr_fuc ref_gpr, bool debug_en = false) {
    const auto reg_names = std::array{
            "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
            "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
            "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
            "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
    };

    bool fail = false;
    // first item is the register index, second item is the value
    auto no_pass_vec = std::vector<std::tuple<uint8_t, uint64_t>>();

    for (auto idx: std::views::iota(0, 32)) {
        auto ref_val = ref_gpr(idx);
        auto dut_val = dut_gpr(idx);
        if (ref_val != dut_val) {
            no_pass_vec.push_back({idx, dut_val});
            fail = true;
        }
    }

    if (debug_en) {
        std::cout << "ref reg\n";
        for (auto idx: std::views::iota(0, 8)) {
            std::cout << std::format("x{}: 0x{:016x} x{}: 0x{:016x} x{}: 0x{:016x} x{}: 0x{:016x}\n",
                                     idx * 4, ref_gpr(idx * 4),
                                     idx * 4 + 1, ref_gpr(idx * 4 + 1),
                                     idx * 4 + 2, ref_gpr(idx * 4 + 2),
                                     idx * 4 + 3, ref_gpr(idx * 4 + 3));
        }
        std::cout << "dut reg\n";
        for (auto idx: std::views::iota(0, 8)) {
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

bool DiffTest::check_pc(uint64_t ref_pc, uint64_t dut_pc) {
    bool fail = false;
    if (ref_pc != dut_pc) {
        std::cout << std::format("pc mismatch: ref: 0x{:016x}, dut: 0x{:016x}\n", ref_pc, dut_pc);
        fail = true;
    }
    return fail;
}
