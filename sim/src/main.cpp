#include <ranges>
#include "AMKBDDev.h"
#include "AMRTCDev.h"
#include "AMUartDev.h"
#include "AMVGADev.h"
#include "CLI/CLI.hpp"
#include "DeviceMange.h"
#include "difftest.hpp"
#include "SimBase.h"
#include "include/SramMemoryDev.h"
#include "spdlog/async.h"
#include "spdlog/spdlog.h"
#include "spdlog/sinks/rotating_file_sink.h"
#include "spdlog/sinks/stdout_color_sinks.h"
constexpr auto MEM_BASE = 0x80000000L;
constexpr auto MEM_SIZE = 0x8000000L;
constexpr auto DEVICE_BASE = 0xa0000000L;
constexpr auto SERIAL_PORT = DEVICE_BASE + 0x00003f8L;
constexpr auto RTC_ADDR = DEVICE_BASE + 0x0000048L;
constexpr auto KBD_ADDR = DEVICE_BASE + 0x0000060L;
constexpr auto VGACTL_ADDR = DEVICE_BASE + 0x0000100L;
constexpr auto FB_ADDR = DEVICE_BASE + 0x1000000L;
constexpr auto BOOT_PC = 0x80000000L;

int main(int argc, char** argv) {
    std::string file_name;
    bool wave_en = false;
    uint64_t wave_stime = 0;
    bool difftest_en = false;
    bool log_en = false;
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
    app.add_option("--wave_stime", wave_stime, "start wave on N commit")->default_val(0);
    app.add_flag("-d,--difftest", difftest_en, "enable difftest with rv64emu")
       ->default_val(false);
    app.add_flag("--debug", debug_en, "enable debug")->default_val(false);
    app.add_flag("-l,--log", log_en, "enable log")->default_val(false);
    app.add_flag("--vga", vga_en, "enable vga")->default_val(false);

    CLI11_PARSE(app, argc, argv)

    // -----------------------
    // log
    // -----------------------

    spdlog::init_thread_pool(1024 * 32, 1);
    auto console = spdlog::stdout_color_mt("console");
    // create a file rotating logger with 5mb size max and 3 rotated files
    auto _trace = spdlog::create_async<spdlog::sinks::rotating_file_sink_mt>(
        "trace", "trace.txt", 1024 * 1024 * 4, 1);
    auto _ipc = spdlog::create_async<spdlog::sinks::rotating_file_sink_mt>(
        "ipc", "ipc.txt", 1024 * 1024 * 4, 1);

    _trace->set_level(spdlog::level::info);

    if (!log_en) {
        _trace->set_level(spdlog::level::off);
        _ipc->set_level(spdlog::level::off);
    }


    // -----------------------
    // device manager
    // -----------------------
    auto device_manager = SimDevices::DeviceMange();
    auto sim_base = SimBase();
    auto sim_mem = SimDevices::SynReadMemoryDev(MEM_BASE, MEM_SIZE);
    auto sim_am_uart = SimDevices::AMUartDev(SERIAL_PORT);
    auto sim_am_rtc = SimDevices::AMRTCDev(RTC_ADDR);
    auto sim_am_vga = std::optional<SimDevices::AMVGADev>();
    auto sim_am_kbd = std::optional<SimDevices::AMKBDDev>();

    device_manager.add_device(&sim_mem);
    device_manager.add_device(&sim_am_uart);
    device_manager.add_device(&sim_am_rtc);

    if (vga_en) {
        sim_am_vga.emplace(FB_ADDR, VGACTL_ADDR);
        sim_am_kbd.emplace(KBD_ADDR);
        sim_am_vga.value().init_screen("npc_v2_sdl");
        sim_am_kbd.value().create_kdb_thread();
        device_manager.add_device(&sim_am_kbd.value());
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
        sim_base.enable_wave_trace("test.fst", wave_stime);
    }

    sim_base.reset();

    uint64_t clk_num = 0;
    uint64_t commit_num = 0;
    uint8_t no_commit_num = 0;
    uint64_t to_host_check_freq = 0;
    enum SimState_t {
        sim_run,
        sim_stop,
        sim_abort,
        sim_finish
    };
    SimState_t state = sim_run;


    auto start_time = std::chrono::utc_clock::now();
    while (!sim_base.finished() && clk_num < max_cycles && state == sim_run) {
        sim_base.step([&](auto top) -> bool {
            no_commit_num += 1;
            clk_num += 1;
            to_host_check_freq += 1;
            // stop run
            if (no_commit_num > 150) {
                console->critical("no commit for 150 cycles, stop run");
                state = sim_abort;
            }
            // memory
            uint64_t rdata = device_manager.update_outputs();
            const bool device_sucess = device_manager.update_inputs(
                top->io_mem_port_i_raddr, top->io_mem_port_i_rd,
                {
                    top->io_mem_port_i_waddr, top->io_mem_port_i_wdata,
                    top->io_mem_port_i_wstrb
                },
                top->io_mem_port_i_we);

            if (!device_sucess) {
                console->critical("device error at pc: 0x{:016x}\n",
                                  sim_base.get_pc());
                state = sim_abort;
            }

            top->io_mem_port_o_rdata = rdata;

            // diff test
            if (top->io_difftest_valid) {
                const auto step_num = top->io_difftest_bits_commited_num;
                const auto has_exception = top->io_difftest_bits_exception_valid;
                const auto has_interrupt = top->io_difftest_bits_has_interrupt;
                const auto cause = top->io_difftest_bits_exception_cause;
                const auto has_mmio = top->io_difftest_bits_contain_mmio;
                const auto has_csr_skip = top->io_difftest_bits_csr_skip;
                const auto is_rvc = top->io_difftest_bits_is_rvc;
                const auto pc = top->io_difftest_bits_pc;
                const auto next_pc = pc + (is_rvc ? 2 : 4); // for diff skip

                commit_num += step_num;
                no_commit_num = 0;
                _trace->trace("pc 0x{:016x}",
                              sim_base.get_pc());

                if (has_interrupt & has_exception || has_exception & has_mmio || has_exception & has_csr_skip ||
                    has_mmio & has_csr_skip || has_interrupt & has_mmio || has_interrupt & has_csr_skip) {
                    console->critical("exception and interrupt and mmio at the same time");
                    console->critical("has_interrupt: {}, has_exception: {}, has_mmio: {}, "
                                      "has_csr_skip: {}\n",
                                      has_interrupt, has_exception, has_mmio, has_csr_skip);
                    state = sim_abort;
                }


                if (top->io_difftest_bits_exception_valid) {
                    _trace->info("exception cause 0x{:x},pc 0x{:016x}\n",
                                 cause, sim_base.get_pc());
                    if (am_en && cause == 3) {
                        // ebreak
                        console->info("AM exit");
                        state = sim_stop;
                    }
                }
                if (difftest_en & diff_ref.has_value()) {
                    // TODO: NOT IMPLEMENTED
                    if (has_mmio || has_csr_skip) {
                        MY_ASSERT(top->io_difftest_bits_exception_valid == 0,
                                  "mmio and exception at the same time");
                        _trace->info("skip mmio at pc: 0x{:016x},next pc: 0x{:016x}",
                                     sim_base.get_pc(), next_pc);
                        diff_ref->ref_skip(
                            [&](const size_t idx) { return sim_base.get_reg(idx); },
                            next_pc);
                    }
                    else {
                        diff_ref->step(step_num);
                        if (has_interrupt) {
                            _trace->info("has_interrupt at pc: 0x{:016x},cause: 0x{:8x}",
                                         sim_base.get_pc(), cause);
                            diff_ref->raise_intr(cause & 0xffff);
                        }
                        const bool pc_mismatch = diff_ref->check_pc(diff_ref->get_pc(),
                                                                    sim_base.get_pc(), debug_en);
                        const bool gpr_mismatch = diff_ref->check_gprs(
                            [&](const size_t idx) { return sim_base.get_reg(idx); },
                            [&](const size_t idx) { return diff_ref->get_reg(idx); }, debug_en);
                        const bool csr_mismatch = diff_ref->check_csrs(
                            [&](const size_t idx) { return sim_base.get_csr(idx); }, debug_en);
                        const bool mismatch = pc_mismatch | gpr_mismatch | csr_mismatch;

                        if (mismatch) {
                            console->critical("DiffTest mismatch");
                            console->critical("pc mismatch: ref: 0x{:016x}, dut: "
                                              "0x{:016x}\n\n",
                                              diff_ref->get_pc(), sim_base.get_pc());
                            state = sim_abort;
                        }
                    }
                }
                return false;
            }
            // for riscof and riscv-tests, use to_host to communicate with simulation
            // environment
            if (to_host_check_freq > 1024) {
                _ipc->info("clk_num: {}, commit_num: {}, IPC: {} \n",
                           clk_num, commit_num,
                           static_cast<double_t>(commit_num) / static_cast<double_t>(clk_num + 1)
                );
                to_host_check_freq = 0;
                sim_mem.check_to_host([&] {
                    state = sim_stop;
                    console->info("Write tohost at pc: 0x{:016x}\n",
                                  sim_base.get_pc());
                });
            }

            return false;
        });
    }

    auto time_end = std::chrono::utc_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(time_end - start_time);


    if (dump_signature_file.has_value()) {
        sim_mem.dump_signature(dump_signature_file.value());
    }


    const auto bp_hit = sim_base.top->io_perf_monitor_bp_hit_counter;
    const auto bp_num = sim_base.top->io_perf_monitor_bp_num_counter;
    const auto bp_rate = static_cast<double_t>(bp_hit) / static_cast<double_t>(bp_num + 1);
    const auto icache_hit = sim_base.top->io_perf_monitor_icache_hit_counter;
    const auto icache_num = sim_base.top->io_perf_monitor_icache_num_counter;
    const auto icache_rate = static_cast<double_t>(icache_hit) / static_cast<double_t>(icache_num + 1);


    console->info("icache_hit: {}, icache_num: {}, icache_rate: {}", icache_hit, icache_num, icache_rate);
    console->info("bp_hit: {}, bp_num: {}, bp_rate: {}", bp_hit, bp_num, bp_rate);
    // add one to avoid div zero
    console->info("clk_num: {}, commit_num: {}, IPC: {}, SimSpeed: {} insts/seconds",
                  clk_num, commit_num,
                  static_cast<double_t>(commit_num) / static_cast<double_t>(clk_num + 1),
                  commit_num * 1000 / (duration.count() + 1));


    bool success = !am_en || sim_base.get_reg(10) == 0;

    // zero means success
    bool return_code = !(success && state != sim_abort);

    return return_code;
}
