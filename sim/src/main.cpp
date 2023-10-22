#include <iostream>
#include <ranges>
#include "simbase.hpp"
#include "include/SramMemoryDev.h"
#include "difftest.hpp"
#include "CLI/CLI.hpp"


int main(int argc, char **argv) {

    constexpr auto MEM_BASE = 0x80000000L;
    constexpr auto MEM_SIZE = 0x800000L;
    constexpr auto BOOT_PC = 0x80000000L;

    std::string file_name;
    bool wave_en = false;
    bool difftest_en = false;
    bool am_en = false;
    bool debug_en = false;
    long max_cycles = 5000;
    std::optional<std::string> dump_signature_file;

    CLI::App app{"Simulator for RISC-V"};
    app.add_option("-f,--file", file_name, "bin/elf file load to the ram")->required();
    app.add_option("-s,--signature", dump_signature_file, "dump signature file(for riscof)")->default_val(std::nullopt);
    app.add_option("--clk", max_cycles, "max cycles")->default_val(50000);
    app.add_flag("--am", am_en, "enable am")->default_val(false);
    app.add_flag("-w,--wave", wave_en, "enable wave trace")->default_val(false);
    app.add_flag("-d,--difftest", difftest_en, "enable difftest with rv64emu")->default_val(false);
    app.add_flag("--debug", debug_en, "enable debug")->default_val(false);


    CLI11_PARSE(app, argc, argv);
    auto sim_base = SimBase();
    auto sim_mem = SimDevices::SynReadMemoryDev(MEM_SIZE);

    auto createDiffTest = [&]() -> std::optional<DiffTest> {
        return difftest_en ? std::make_optional<DiffTest>(MEM_BASE, MEM_SIZE, BOOT_PC) : std::nullopt;
    };
    auto diff_ref = createDiffTest();

    if (diff_ref.has_value()) {
        diff_ref->load_file(file_name.c_str());
    }


    sim_mem.load_file(file_name.c_str());

    if (wave_en) {
        sim_base.enable_wave_trace("test.fst");
    }


    sim_base.reset();

    uint64_t clk_num = 0;
    uint64_t commit_num = 0;
    bool sim_abort = false;
    while (!sim_base.finished() && (clk_num < max_cycles)) {
        sim_base.step([&](auto top) -> bool {
            clk_num += 1;
            // memory
            uint64_t rdata = sim_mem.update_outputs();
            sim_mem.update_inputs(
                    top->io_mem_port_i_raddr - 0x80000000,
                    top->io_mem_port_i_rd,
                    {
                            top->io_mem_port_i_waddr - 0x80000000,
                            top->io_mem_port_i_wdata,
                            top->io_mem_port_i_wstrb
                    },
                    top->io_mem_port_i_we
            );
            top->io_mem_port_o_rdata = rdata;

            // diff test
            if (top->io_difftest_valid) {
                int step_num = top->io_difftest_bits_commited_num;
                commit_num += step_num;

                if (top->io_difftest_bits_exception_valid) {
                    auto cause = top->io_difftest_bits_exception_cause;
                    std::cout << std::format("exception cause 0x{:x},pc 0x{:016x}\n",
                                             cause, sim_base.get_pc());
                    if (cause != 3) {
                        sim_abort = true;
                    }
                    return true;
                } else if (difftest_en & diff_ref.has_value()) {
                    diff_ref->step(step_num);
                    bool pc_fail = diff_ref->check_pc(diff_ref->get_pc(), sim_base.get_pc(), debug_en);
                    bool gpr_fail = diff_ref->check_gprs(
                            [&](size_t idx) { return sim_base.get_reg(idx); },
                            [&](size_t idx) { return diff_ref->get_reg(idx); },
                            debug_en
                    );
                    if ((pc_fail | gpr_fail)) {
                        std::cout
                                << std::format("pc mismatch: ref: 0x{:016x}, dut: 0x{:016x}\n\n", diff_ref->get_pc(),
                                               sim_base.get_pc());
                        sim_abort = true;
                    };
                    return (pc_fail | gpr_fail);
                }
                return false;
            };


            if ((clk_num % 1024) == 0) {
                bool to_host_ret = false;
                sim_mem.check_to_host([&]() {
                    to_host_ret = true;
                });
                if (to_host_ret) {
                    std::cout << std::format("to host at pc: 0x{:016x}\n", sim_base.get_pc());
                    return true;
                }
            }

            return false;
        });
    }


    if (dump_signature_file.has_value()) {
        sim_mem.dump_signature(dump_signature_file.value());
    }


    std::cout << std::format("clk_num: {}, commit_num: {}, IPC: {}\n", clk_num, commit_num,
                             double_t(commit_num) / clk_num);


    bool success = am_en ? sim_base.get_reg(10) == 0 : true;


    // zero means success
    bool return_code = !(success & !sim_abort);

    return return_code;
}


