#include <iostream>
#include <ranges>
#include "simbase.hpp"
#include "SynReadMemorySim.hpp"
#include "difftest.hpp"
#include "CLI/CLI.hpp"

void check_regs(std::span<uint64_t> ref_regs, std::span<uint64_t> dut_regs) {
    for (auto idx: std::views::iota(0, 32)) {
        auto ref_val = ref_regs[idx];
        auto dut_val = dut_regs[idx];
        if (ref_val != dut_val) {
            std::cout << std::format("reg {} mismatch: ref: 0x{:016x}, dut: 0x{:016x}", idx, ref_val, dut_val);
        }
    }
}


int main([[maybe_unused]] int argc, [[maybe_unused]] char **argv) {

    std::string file_name;
    bool wave_en = false;

    CLI::App app{"Simulator for RISC-V"};
    app.add_option("-f", file_name, "image file load to the ram")->required();
    app.add_flag("-w", wave_en, "enable wave trace")->default_val(false);

    CLI11_PARSE(app, argc, argv);


    std::cout << "hello world!" << std::endl;


    auto sim_base = SimBase();
    auto sim_mem = SynReadMemorySim(0x10000);
    auto difftest_ref = DiffTest(0x80000000, 0x10000, 0x80000000);


    if (wave_en) {
        sim_base.enable_wave_trace("test.fst");
    }

    sim_mem.load_file(file_name);
    difftest_ref.load_file(file_name.c_str());


    sim_base.reset();

    uint64_t clk_num = 0;
    uint64_t commit_num = 0;
    bool sim_abort = false;
    while (!sim_base.finished()) {
        clk_num++;
        sim_base.step([&](auto top) -> bool {
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
                difftest_ref.step(step_num);

                if (top->io_difftest_bits_exception_valid) {
                    auto cause = top->io_difftest_bits_exception_cause;
                    std::cout << std::format("exception cause 0x{:x},pc 0x{:016x}\n",
                                             cause, sim_base.get_pc());
                    if (cause != 3) {
                        sim_abort = true;
                    }


                    return true;
                } else {
                    bool pc_fail = difftest_ref.check_pc(difftest_ref.get_pc(), sim_base.get_pc());
                    bool gpr_fail = difftest_ref.check_gprs(
                            [&](size_t idx) { return sim_base.get_reg(idx); },
                            [&](size_t idx) { return difftest_ref.get_reg(idx); }
                    );
                    if ((pc_fail | gpr_fail)) {
                        std::cout
                                << std::format("pc mismatch: ref: 0x{:016x}, dut: 0x{:016x}\n\n", difftest_ref.get_pc(),
                                               sim_base.get_pc());
                    };
                    return (pc_fail | gpr_fail);
                }
            };

            return false;
        });
    }


    std::cout << std::format("clk_num: {}, commit_num: {}, IPC: {}\n", clk_num / 2, commit_num,
                             double_t(commit_num) / (clk_num / 2));


    auto success = sim_base.get_reg(10) == 0;


    // return zero if success
    return !(success & !sim_abort);
}


