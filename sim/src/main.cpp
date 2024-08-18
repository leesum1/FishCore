#include "AMKBDDev.h"
#include "AMRTCDev.h"
#include "AMUartDev.h"
#include "AMVGADev.h"
#include "CLI/CLI.hpp"
#include "DeviceMange.h"
#include "SimBase.h"
#include "difftest.hpp"
#include "include/Itrace.h"
#include "include/SramMemoryDev.h"
#include "spdlog/async.h"
#include "spdlog/sinks/basic_file_sink.h"
#include "spdlog/sinks/rotating_file_sink.h"
#include "spdlog/sinks/stdout_color_sinks.h"
#include "spdlog/spdlog.h"
#include <PerfMonitor.h>
#include <cstdint>
#include <cstdio>
#include <ranges>
#include <sys/types.h>
constexpr auto MEM_BASE = 0x80000000L;
constexpr auto MEM_SIZE = 0x8000000L;
constexpr auto DEVICE_BASE = 0xa0000000L;
constexpr auto SERIAL_PORT = DEVICE_BASE + 0x00003f8L;
constexpr auto RTC_ADDR = DEVICE_BASE + 0x0000048L;
constexpr auto KBD_ADDR = DEVICE_BASE + 0x0000060L;
constexpr auto VGACTL_ADDR = DEVICE_BASE + 0x0000100L;
constexpr auto FB_ADDR = DEVICE_BASE + 0x1000000L;
constexpr auto BOOT_PC = 0x80000000L;

int main(int argc, char **argv) {
  std::string image_name;
  bool wave_en = false;
  uint64_t wave_stime = 0;
  bool difftest_en = false;
  bool itrace_log_en = false;
  bool perf_trace_log_en = false;
  bool difftest_log_en = false;
  bool am_en = false;
  bool vga_en = false;

  long max_cycles = 50000;
  std::optional<std::string> dump_signature_file;

  CLI::App app{"Simulator for RISC-V"};
  app.add_option("-f,--file", image_name, "bin/elf file load to the ram")
      ->required();
  app.add_option("-s,--signature", dump_signature_file,
                 "dump signature file(for riscof)")
      ->default_val(std::nullopt);
  app.add_option("--clk", max_cycles, "max cycles")->default_val(50000);
  app.add_flag("--am", am_en, "enable am")->default_val(false);
  app.add_flag("-w,--wave", wave_en, "enable wave trace")->default_val(false);
  app.add_option("--wave_stime", wave_stime, "start wave on N commit")
      ->default_val(0);
  app.add_flag("-d,--difftest", difftest_en, "enable difftest with rv64emu")
      ->default_val(false);
  // log options
  app.add_flag("--difftest_log", difftest_log_en, "enable log")
      ->default_val(false);
  app.add_flag("--itrace", itrace_log_en, "enable instruction trace")
      ->default_val(false);
  app.add_flag("--perf_trace", perf_trace_log_en, "enable perf trace")
      ->default_val(false);
  // device options
  app.add_flag("--vga", vga_en, "enable am vga")->default_val(false);

  CLI11_PARSE(app, argc, argv)

  // -----------------------
  // log
  // -----------------------

  spdlog::init_thread_pool(1024 * 32, 1);
  auto console = spdlog::stdout_color_mt("console");
  // Create another file logger that overwrites the file each time the program
  // runs
  auto diff_trace = spdlog::create_async<spdlog::sinks::basic_file_sink_mt>(
      "diff_trace", "diff_trace.txt", true); // true for truncate (overwrite)
  auto perf_trace = spdlog::create_async<spdlog::sinks::basic_file_sink_mt>(
      "perf_trace", "perf_trace.txt", false); // false for append
  auto itrace_log = spdlog::create_async<spdlog::sinks::basic_file_sink_mt>(
      "itrace", "itrace.txt", true); // true for truncate (overwrite)

  diff_trace->set_level(spdlog::level::info);
  perf_trace->set_level(spdlog::level::info);
  itrace_log->set_level(spdlog::level::info);
  itrace_log->set_pattern("%v");
  perf_trace->set_pattern("%v");
  diff_trace->set_pattern("%v");
  if (!difftest_log_en) {
    diff_trace->set_level(spdlog::level::off);
  }
  if (!itrace_log_en) {
    itrace_log->set_level(spdlog::level::off);
  }
  if (!perf_trace_log_en) {
    perf_trace->set_level(spdlog::level::off);
  }

  // -----------------------
  // simulator
  // -----------------------

  auto sim_base = SimBase();

  // -----------------------
  // Device Manager
  // -----------------------
  auto device_manager = SimDevices::DeviceMange();
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

  sim_base.add_after_clk_rise_task(
      {[&] {
         const uint64_t rdata = device_manager.update_outputs();
         const auto top = sim_base.top;
         const bool device_sucess = device_manager.update_inputs(
             top->io_mem_port_i_raddr, top->io_mem_port_i_rd,
             {.waddr = top->io_mem_port_i_waddr,
              .wdata = top->io_mem_port_i_wdata,
              .wstrb = top->io_mem_port_i_wstrb},
             top->io_mem_port_i_we);

         if (!device_sucess) {
           console->critical("device error at pc: 0x{:016x}\n",
                             sim_base.get_pc());
           sim_base.set_state(SimBase::sim_abort);
         }

         top->io_mem_port_o_rdata = rdata;
       },
       "update devices", 0});

  // sim_base.add_after_clk_rise_task({
  //     [&] {
  //       const auto top = sim_base.top;
  //       const uint64_t clk_num = sim_base.cycle_num;

  //       static uint64_t halted_clk = 0;
  //       static bool is_halted = false;
  //       static uint32_t cur_halt_count = 1;

  //       // 发出一个周期的 halt 请求
  //       if (clk_num == 10000 * cur_halt_count) {
  //         top->io_debug_halt_req_valid = 1;
  //         top->io_debug_halt_req_bits = 1;
  //       }
  //       if (clk_num == 10000 * cur_halt_count + 1) {
  //         top->io_debug_halt_req_valid = 0;
  //         top->io_debug_halt_req_bits = 0;
  //       }

  //       if (top->io_debug_state_regs_is_halted && !is_halted) {
  //         perf_trace->info("halted at pc: 0x{:016x}\n", sim_base.get_pc());
  //         halted_clk = clk_num;
  //         is_halted = true;
  //       }

  //       if (is_halted) {

  //         // 发出一个周期的 resume 请求
  //         if (clk_num == halted_clk + 200) {
  //           top->io_debug_resume_req_valid = 1;
  //           top->io_debug_resume_req_bits = 1;
  //         }
  //         if (clk_num == halted_clk + 201) {
  //           top->io_debug_resume_req_valid = 0;
  //           top->io_debug_resume_req_bits = 0;
  //         }

  //         // 等待处理器恢复
  //         if (top->io_debug_state_regs_is_halted == 0) {
  //           perf_trace->info("resume at pc: 0x{:016x} clock:{}\n",
  //                         sim_base.get_pc(), clk_num);
  //           is_halted = false;
  //           cur_halt_count++;
  //         }
  //       }
  //     },
  //     "debug test",
  //     0,
  // });

  //   sim_base.add_after_clk_rise_task({
  //     [&] {
  //       const auto top = sim_base.top;
  //       const uint64_t clk_num = sim_base.cycle_num;

  //       static uint64_t halted_clk = 0;
  //       static bool is_halted = false;
  //       static uint32_t cur_halt_count = 1;

  //       static bool halt_flags[2] = {false, false};
  //       // 发出一个周期的 halt 请求
  //       if (clk_num == 200 && halt_flags[0] == false) {
  //         halt_flags[0] = true;
  //         top->io_debug_halt_req_valid = 1;
  //         top->io_debug_halt_req_bits = 1;
  //       }
  //       if (clk_num == 201 && halt_flags[1] == false) {
  //         halt_flags[1] = true;
  //         top->io_debug_halt_req_valid = 0;
  //         top->io_debug_halt_req_bits = 0;
  //       }

  //       if (top->io_debug_state_regs_is_halted && !is_halted) {
  //         perf_trace->info("halted at pc: 0x{:016x}\n", sim_base.get_pc());
  //         halted_clk = clk_num;
  //         is_halted = true;
  //       }

  //       if (is_halted) {
  //         // 发出一个周期的 resume 请求, bits 设为 1 表示单步执行
  //         if (clk_num == halted_clk + 200) {
  //           top->io_debug_resume_req_valid = 1;
  //           top->io_debug_resume_req_bits = 1;
  //         }
  //         if (clk_num == halted_clk + 201) {
  //           top->io_debug_resume_req_valid = 0;
  //         }

  //         // 等待处理器恢复
  //         if (top->io_debug_state_regs_is_halted == 0) {
  //           perf_trace->info("resume at pc: 0x{:016x} clock:{}\n",
  //                         sim_base.get_pc(), clk_num);
  //           is_halted = false;
  //           cur_halt_count++;
  //         }
  //       }
  //     },
  //     "stepi test",
  //     0,
  // });

  // -----------------------
  // Perf Monitor
  // -----------------------

  auto perf_monitor = PerfMonitor();
  perf_monitor.add_perf_counter(
      {"bp_f1", &sim_base.top->io_perf_monitor_bp_f1_hit_counter,
       &sim_base.top->io_perf_monitor_bp_f1_num_counter});
  perf_monitor.add_perf_counter(
      {"bp", &sim_base.top->io_perf_monitor_bp_hit_counter,
       &sim_base.top->io_perf_monitor_bp_num_counter});
  perf_monitor.add_perf_counter(
      {"icache", &sim_base.top->io_perf_monitor_icache_hit_counter,
       &sim_base.top->io_perf_monitor_icache_num_counter});
  perf_monitor.add_perf_counter(
      {"dcache", &sim_base.top->io_perf_monitor_dcache_hit_counter,
       &sim_base.top->io_perf_monitor_dcache_num_counter});
  perf_monitor.add_perf_counter(
      {"itlb", &sim_base.top->io_perf_monitor_itlb_hit_counter,
       &sim_base.top->io_perf_monitor_itlb_num_counter});
  perf_monitor.add_perf_counter(
      {"dtlb", &sim_base.top->io_perf_monitor_dtlb_hit_counter,
       &sim_base.top->io_perf_monitor_dtlb_num_counter});

  perf_monitor.add_perf_counter(
      {"IPC", &sim_base.commit_num, &sim_base.cycle_num});
  perf_monitor.add_perf_counter(
      {"CPI", &sim_base.cycle_num, &sim_base.commit_num});

  if (perf_trace_log_en) {
    sim_base.add_after_clk_rise_task(
        {[&] { perf_monitor.print_perf_counter(false); }, "perf_monitor",
         8192});
  }

  // -----------------------
  // Exit Condtion Detect
  // -----------------------

  // for riscof and riscv-tests, use to_host to communicate with simulation
  // environment
  sim_base.add_after_clk_rise_task(
      {[&] {
         sim_mem.check_to_host([&] {
           sim_base.set_state(SimBase::sim_stop);
           console->info("Write tohost at pc: 0x{:016x}\n", sim_base.get_pc());
         });
       },
       "to_host_check", 1024});

  sim_base.add_after_clk_rise_task(
      {[&] {
         if (sim_base.not_commit_num > 4096) {
           console->critical(
               "no commit for {} cycles, dead lock at pc: 0x{:016x}\n", 4096,
               sim_base.get_pc());
           sim_base.set_state(SimBase::sim_abort);
         }
       },
       "dead_lock_check", 4096});

  if (am_en) {

    sim_base.add_after_clk_rise_task(
        {[&] {
           const auto top = sim_base.top;
           if (top->io_difftest_bits_exception_valid &&
               top->io_difftest_valid) {
             const auto cause = top->io_difftest_bits_exception_cause;
             if (cause == 3) {
               // ebreak
               console->info("AM exit(ebreak) at pc: 0x{:016x}\n",
                             sim_base.get_pc());
               sim_base.set_state(SimBase::sim_finish);
             }
           }
         },
         "am exit(ebreak) check", 0});
  }

  // -----------------------
  // Difftest
  // -----------------------
  auto diff_ref = std::optional<DiffTest>();
  if (difftest_en) {
    diff_ref.emplace(BOOT_PC, MEM_SIZE, MEM_BASE);
    diff_ref->load_file(image_name.c_str());

    sim_base.add_after_clk_rise_task(
        {[&] {
           const auto top = sim_base.top;
           // diff test
           if (top->io_difftest_valid) {
             const auto step_num = top->io_difftest_bits_commited_num;
             const auto has_exception = top->io_difftest_bits_exception_valid;
             const auto has_interrupt = top->io_difftest_bits_has_interrupt;
             const auto cause = top->io_difftest_bits_exception_cause;
             const auto has_mmio = top->io_difftest_bits_contain_mmio;
             const auto has_csr_skip = top->io_difftest_bits_csr_skip;
             const auto is_rvc = top->io_difftest_bits_last_is_rvc;
             const auto pc = top->io_difftest_bits_last_pc;
             const auto next_pc = pc + (is_rvc ? 2 : 4); // for diff skip

             if (has_interrupt & has_exception || has_exception & has_mmio ||
                 has_exception & has_csr_skip || has_mmio & has_csr_skip ||
                 has_interrupt & has_mmio || has_interrupt & has_csr_skip) {
               console->critical(
                   "exception and interrupt and mmio at the same time");
               console->critical(
                   "has_interrupt: {}, has_exception: {}, has_mmio: {}, "
                   "has_csr_skip: {}\n",
                   has_interrupt, has_exception, has_mmio, has_csr_skip);
               sim_base.set_state(SimBase::sim_abort);
             }

             if (has_mmio || has_csr_skip) {
               diff_trace->info(
                   "skip mmio or csr at pc: 0x{:016x},next pc: 0x{:016x}",
                   sim_base.get_pc(), next_pc);
               diff_ref->ref_skip(
                   [&](const size_t idx) { return sim_base.get_reg(idx); },
                   next_pc);
             } else {
               diff_ref->step(step_num);

               diff_trace->info(
                   "Commit {} inst at pc: 0x{:016x},next pc: 0x{:016x}",
                   step_num, sim_base.get_pc(), next_pc);

               if (has_interrupt) {
                 diff_trace->info(
                     "has_interrupt at pc: 0x{:016x},cause: 0x{:8x}",
                     sim_base.get_pc(), cause);
                 diff_ref->raise_intr(cause & 0xffff);
               }
               const bool pc_mismatch =
                   diff_ref->check_pc(diff_ref->get_pc(), sim_base.get_pc());
               const bool gpr_mismatch = diff_ref->check_gprs(
                   [&](const size_t idx) { return sim_base.get_reg(idx); },
                   [&](const size_t idx) { return diff_ref->get_reg(idx); });
               const bool csr_mismatch = diff_ref->check_csrs(
                   [&](const size_t idx) { return sim_base.get_csr(idx); });
               const bool mismatch = pc_mismatch | gpr_mismatch | csr_mismatch;

               if (mismatch) {
                 console->critical("DiffTest mismatch");
                 console->critical("pc mismatch: ref: 0x{:08x}, dut: "
                                   "0x{:08x}\n\n",
                                   diff_ref->get_pc(), sim_base.get_pc());
                 sim_base.set_state(SimBase::sim_abort);
               }
               diff_trace->info("--------------------End DiffTest at pc: "
                                "0x{:016x}----------------------\n",
                                sim_base.get_pc());
             }
           }
         },
         "difftest", 0});
  }

  // -----------------------
  // Itrace
  // -----------------------

  auto itrace = std::optional<Itrace>();

  if (itrace_log_en) {
    itrace.emplace();
    sim_base.add_after_clk_rise_task(
        {[&] {
           const auto top = sim_base.top;
           if (top->io_difftest_valid) {
             const auto has_exception = top->io_difftest_bits_exception_valid;
             const auto has_interrupt = top->io_difftest_bits_has_interrupt;
             const auto cause = top->io_difftest_bits_exception_cause;
             const auto pc = top->io_difftest_bits_last_pc;

             if (has_exception) {
               itrace->riscv_disasm(top->io_difftest_bits_inst_info_0_inst,
                                    top->io_difftest_bits_last_pc);
               itrace_log->info("⬆️ pc 0x{:08x},exception cause 0x{:x}\n", pc,
                                cause);
             } else if (has_interrupt) {
               itrace->riscv_disasm(top->io_difftest_bits_inst_info_0_inst,
                                    top->io_difftest_bits_last_pc);
               itrace_log->info("⬆️ pc 0x{:08x},interrupt cause 0x{:x}\n", pc,
                                cause);
             } else {
               const auto pc_list = std::array{
                   static_cast<uint64_t>(top->io_difftest_bits_inst_info_0_pc),
                   static_cast<uint64_t>(top->io_difftest_bits_inst_info_1_pc),
               };
               const auto inst_list = std::array{
                   static_cast<uint32_t>(
                       top->io_difftest_bits_inst_info_0_inst),
                   static_cast<uint32_t>(
                       top->io_difftest_bits_inst_info_1_inst),
               };

               for (int i = 0; i < top->io_difftest_bits_commited_num; i++) {
                 itrace->riscv_disasm(inst_list[i], pc_list[i]);
               }
             }
           }
         },
         "Inst Trace", 0});
  }

  // --------------------------
  // Simulator Start Excuting
  // --------------------------
  console->info("Simulator init finished");

  if (wave_en) {
    const auto wave_name = "wave.fst";
    sim_base.enable_wave_trace(wave_name, wave_stime);
    console->info("Wave init finished, File:{}", wave_name);
  }
  sim_mem.load_file(image_name.c_str());
  sim_base.reset();
  console->info("Simulator start");

  auto start_time = std::chrono::utc_clock::now();

  // simulator loop
  while (!sim_base.finished() && sim_base.cycle_num < max_cycles) {
    sim_base.step();
  }

  auto time_end = std::chrono::utc_clock::now();
  auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(
      time_end - start_time);

  if (dump_signature_file.has_value()) {
    sim_mem.dump_signature(dump_signature_file.value());
  }

  console->info(
      "Simulator exit, Simulate {} cycles in {} ms, Speed {:.2f}K inst/second ",
      sim_base.cycle_num, duration.count(),
      static_cast<double>(sim_base.commit_num) / (duration.count() + 1));

  perf_monitor.print_perf_counter(true);

  bool success = !am_en || sim_base.get_reg(10) == 0;

  // zero means success
  bool return_code = !(success && sim_base.get_state() != SimBase::sim_abort);

  return return_code;
}
