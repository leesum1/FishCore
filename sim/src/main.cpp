#include "AMRTCDev.h"
#include "AMUartDev.h"
#include "AMVGADev.h"
#include "CLI/CLI.hpp"
#include "DeviceMange.h"
#include "SimBase.h"
#include "difftest.hpp"
#include "include/SramMemoryDev.h"
#include <iostream>
#include <ranges>

constexpr auto MEM_BASE = 0x80000000L;
constexpr auto MEM_SIZE = 0x8000000L;
constexpr auto DEVICE_BASE = 0xa0000000L;
constexpr auto SERIAL_PORT = DEVICE_BASE + 0x00003f8L;
constexpr auto RTC_ADDR = DEVICE_BASE + 0x0000048L;
constexpr auto VGACTL_ADDR = DEVICE_BASE + 0x0000100L;
constexpr auto FB_ADDR = DEVICE_BASE + 0x1000000L;
constexpr auto BOOT_PC = 0x80000000L;

int main(int argc, char **argv) {
    std::string file_name;
    bool wave_en = false;
    bool difftest_en = false;
    bool am_en = false;
    bool debug_en = false;
    bool vga_en = false;
    long max_cycles = 5000;
    std::optional<std::string> dump_signature_file;

    CLI::App app{"Simulator for RISC-V"};
    app.add_option("-f,--file", file_name, "bin/elf file load to the ram")
            ->required();
    app.add_option("-s,--signature", dump_signature_file,
                   "dump signature file(for riscof)")
            ->default_val(std::nullopt);
    app.add_option("--clk", max_cycles, "max cycles")->default_val(50000);
    app.add_flag("--am", am_en, "enable am")->default_val(false);
    app.add_flag("-w,--wave", wave_en, "enable wave trace")->default_val(false);
    app.add_flag("-d,--difftest", difftest_en, "enable difftest with rv64emu")
            ->default_val(false);
    app.add_flag("--debug", debug_en, "enable debug")->default_val(false);
    app.add_flag("--vga", vga_en, "enable vga")->default_val(false);

    CLI11_PARSE(app, argc, argv)

    auto device_manager = SimDevices::DeviceMange();
    auto sim_base = SimBase();
    auto sim_mem = SimDevices::SynReadMemoryDev(MEM_BASE, MEM_SIZE);
    auto sim_am_uart = SimDevices::AMUartDev(SERIAL_PORT);
    auto sim_am_rtc = SimDevices::AMRTCDev(RTC_ADDR);
    auto sim_am_vga = std::optional<SimDevices::AMVGADev>();

    device_manager.add_device(&sim_mem);
    device_manager.add_device(&sim_am_uart);
    device_manager.add_device(&sim_am_rtc);

    if (vga_en) {
        sim_am_vga.emplace(FB_ADDR, VGACTL_ADDR);
        sim_am_vga.value().init_screen("npc_v2_sdl");
        device_manager.add_device(&sim_am_vga.value());
    }

    device_manager.print_device_info();

    auto createDiffTest = [&]() -> std::optional<DiffTest> {
        return difftest_en
               ? std::make_optional<DiffTest>(MEM_BASE, MEM_SIZE, BOOT_PC)
               : std::nullopt;
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
    uint8_t no_commit_num = 0;
    uint64_t to_host_check_freq = 0;

    bool sim_abort = false;
    while (!sim_base.finished() && (clk_num < max_cycles)) {
        sim_base.step([&](auto top) -> bool {
            no_commit_num += 1;
            clk_num += 1;
            to_host_check_freq +=1;
            // stop run
            if (no_commit_num > 150) {
                return true;
            }
            // memory
            uint64_t rdata = device_manager.update_outputs();
            device_manager.update_inputs(
                    top->io_mem_port_i_raddr, top->io_mem_port_i_rd,
                    {top->io_mem_port_i_waddr, top->io_mem_port_i_wdata,
                     top->io_mem_port_i_wstrb},
                    top->io_mem_port_i_we);
            top->io_mem_port_o_rdata = rdata;

            // diff test
            if (top->io_difftest_valid) {
                int step_num = top->io_difftest_bits_commited_num;
                commit_num += step_num;
                no_commit_num = 0;
                if (top->io_difftest_bits_exception_valid) {
                    auto cause = top->io_difftest_bits_exception_cause;

                    auto valid_cause = std::array{0, 2, 3, 4, 6, 8, 11};
                    if (std::ranges::find(valid_cause, cause) == valid_cause.end()) {
                        std::cout << std::format("exception cause 0x{:x},pc 0x{:016x}\n",
                                                 cause, sim_base.get_pc());
                        sim_abort = true;
                        return true;
                    }
                    if (cause == 3 && am_en) {
                        return true;
                    }
                }
                if (difftest_en & diff_ref.has_value()) {

                    // TODO: NOT IMPLEMENTED
                    if (top->io_difftest_bits_contain_mmio) {
                        //    std::cout << std::format("mmio at pc: 0x{:016x}\n",
                        //                         sim_base.get_pc());
                        MY_ASSERT(top->io_difftest_bits_exception_valid == 0,
                                  "mmio and exception at the same time");
                        diff_ref->ref_skip(
                                [&](size_t idx) { return sim_base.get_reg(idx); },
                                sim_base.get_pc() + 4);
                    } else {
                        diff_ref->step(step_num);
                        bool pc_mismatch = diff_ref->check_pc(diff_ref->get_pc(),
                                                              sim_base.get_pc(), debug_en);
                        bool gpr_mismatch = diff_ref->check_gprs(
                                [&](size_t idx) { return sim_base.get_reg(idx); },
                                [&](size_t idx) { return diff_ref->get_reg(idx); }, debug_en);
                        bool csr_mismatch = diff_ref->check_csrs(
                                [&](size_t idx) { return sim_base.get_csr(idx); }, debug_en);
                        bool mismatch = pc_mismatch | gpr_mismatch | csr_mismatch;

                        if (mismatch) {
                            std::cout << std::format("pc mismatch: ref: 0x{:016x}, dut: "
                                                     "0x{:016x}\n\n",
                                                     diff_ref->get_pc(), sim_base.get_pc());
                            sim_abort = true;
                        }
                        return mismatch;
                    }
                }
                return false;
            }
            // for riscof and riscv-tests, use to_host to communicate with simulation
            // environment
            if (to_host_check_freq > 1024) {
                bool to_host_ret = false;
                to_host_check_freq = 0;
                sim_mem.check_to_host([&]() { to_host_ret = true; });
                if (to_host_ret) {
                    std::cout << std::format("to host at pc: 0x{:016x}\n",
                                             sim_base.get_pc());
                    return true;
                }
            }

            return false;
        });
    }

    if (dump_signature_file.has_value()) {
        sim_mem.dump_signature(dump_signature_file.value());
    }

    std::cout << std::format("clk_num: {}, commit_num: {}, IPC: {}\n", clk_num,
                             commit_num, double_t(commit_num) / clk_num);

    bool success = !am_en || sim_base.get_reg(10) == 0;

    // zero means success
    bool return_code = !(success & !sim_abort);

    return return_code;
}
