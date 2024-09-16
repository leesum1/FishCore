#include "AMKBDDev.h"
#include "AMRTCDev.h"
#include "AMUartDev.h"
#include "AMVGADev.h"
#include "CLI/CLI.hpp"
#include "DeviceMange.h"
#include "RemoteBitBang.h"
#include "SimBase.h"
#include "difftest.hpp"
#include "include/AllTask.h"
#include "include/Itrace.h"
#include "include/SramMemoryDev.h"
#include "spdlog/async.h"
#include "spdlog/sinks/basic_file_sink.h"
#include "spdlog/sinks/stdout_color_sinks.h"
#include <PerfMonitor.h>
#include <cassert>
#include <csignal>
#include <cstdint>
#include <cstdio>

constexpr auto MEM_BASE = 0x80000000L;
constexpr auto MEM_SIZE = 0x8000000L;
constexpr auto DEVICE_BASE = 0xa0000000L;
constexpr auto SERIAL_PORT = DEVICE_BASE + 0x00003f8L;
constexpr auto RTC_ADDR = DEVICE_BASE + 0x0000048L;
constexpr auto KBD_ADDR = DEVICE_BASE + 0x0000060L;
constexpr auto VGACTL_ADDR = DEVICE_BASE + 0x0000100L;
constexpr auto FB_ADDR = DEVICE_BASE + 0x1000000L;
constexpr auto BOOT_PC = 0x80000000L;

static bool is_exit = false;

void signal_handler(int signal) {
  if (signal == SIGINT) {
    std::cout << "Ctrl+C detected. Exiting gracefully..." << std::endl;
    // 在这里执行清理操作
    // ...
    is_exit = true;
  }
}

void read_from_pipe(int pipefd, std::ofstream &file) {
  char buffer[1024];
  ssize_t bytesRead;
  while ((bytesRead = read(pipefd, buffer, sizeof(buffer) - 1)) > 0) {
    buffer[bytesRead] = '\0';
    file << buffer;
    file.flush(); // 手动刷新文件流
  }
}

int main(int argc, char **argv) {

  // // 将 stderr 重定向到文件, 以便将 verilator 的打印信息写入文件
  // FILE *err_file = freopen("error_output.txt", "a", stderr);

  // 打开一个文件用于写入
  std::ofstream file("error_output.txt", std::ios::trunc);

  if (!file.is_open()) {
    std::cerr << "Failed to open the file for writing." << std::endl;
    return EXIT_FAILURE;
  }

  // 创建管道
  int pipefd[2];
  if (pipe(pipefd) == -1) {
    std::cerr << "Failed to create pipe." << std::endl;
    return EXIT_FAILURE;
  }

  // 备份原始的 stderr
  int original_stderr = dup(STDERR_FILENO);

  // 将 stderr 重定向到管道
  dup2(pipefd[1], STDERR_FILENO);
  close(pipefd[1]);

  // 启动一个线程从管道读取数据并写入文件
  std::thread pipeReaderThread(read_from_pipe, pipefd[0], std::ref(file));

  pipeReaderThread.detach();

  // 注册信号处理函数
  if (signal(SIGINT, signal_handler) == SIG_ERR) {
    std::printf("Error setting up signal handler");
    return EXIT_FAILURE;
  }

  std::cerr << "This is a test message." << std::endl;

  std::string image_name;
  bool wave_en = false;
  uint64_t wave_stime = 0;
  bool difftest_en = false;
  bool itrace_log_en = false;
  bool perf_trace_log_en = false;
  bool difftest_log_en = false;
  bool am_en = false;
  bool vga_en = false;
  bool rbb_en = false;
  bool to_host_check_en = false;
  bool corotinue_en = false;

  long max_cycles = 50000;
  int rbb_port = 23456;
  std::optional<std::string> dump_signature_file = std::nullopt;

  CLI::App app{"Simulator for RISC-V"};
  app.add_option("-f,--file", image_name, "bin/elf file load to the ram")
      ->required();
  app.add_option("-s,--signature", dump_signature_file,
                 "dump signature file(for riscof)");

  app.add_option("--clk", max_cycles, "max cycles")->default_val(50000);
  app.add_flag("--am", am_en, "enable am")->default_val(false);
  app.add_flag("-w,--wave", wave_en, "enable wave trace")->default_val(false);
  app.add_option("--wave_stime", wave_stime, "start wave on N commit")
      ->default_val(0);
  app.add_flag("-d,--difftest", difftest_en, "enable difftest with rv64emu")
      ->default_val(false);
  // log options
  app.add_flag("--diff-log", difftest_log_en, "enable log")->default_val(false);
  app.add_flag("--itrace", itrace_log_en, "enable instruction trace")
      ->default_val(false);
  app.add_flag("--perf-trace", perf_trace_log_en, "enable perf trace")
      ->default_val(false);
  // device options
  app.add_flag("--vga", vga_en, "enable am vga")->default_val(false);

  // remote bitbang options
  app.add_flag("--rbb", rbb_en, "enable remote bitbang")->default_val(false);
  app.add_option("--rbb-port", rbb_port, "remote bitbang port")
      ->default_val(23456);

  // tohost check
  app.add_flag("--tohost-check", to_host_check_en, "enable to_host check")
      ->default_val(false);
  app.add_flag("-c,--corotinue", corotinue_en, "enable corotinue task")
      ->default_val(false);

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
  if (!difftest_log_en) {
    diff_trace->set_level(spdlog::level::off);
  }
  if (!itrace_log_en) {
    itrace_log->set_level(spdlog::level::off);
  }
  if (!perf_trace_log_en) {
    perf_trace->set_level(spdlog::level::off);
  }

  itrace_log->set_pattern("%v");
  perf_trace->set_pattern("%v");
  diff_trace->set_pattern("%v");

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

  // -----------------------
  // SOC UART IO
  // -----------------------

  task_uart_io(sim_base);

  // -----------------------
  // Perf Monitor
  // -----------------------

  auto perf_monitor = PerfMonitor();
  task_perfmonitor(sim_base, perf_monitor, perf_trace_log_en);

  // -----------------------
  // Exit Condtion Detect
  // -----------------------

  task_tohost_check(sim_base, sim_mem, to_host_check_en);
  task_deadlock_check(sim_base);
  task_am_ebreak_check(sim_base, am_en);

  // -----------------------
  // Difftest
  // -----------------------
  auto diff_ref = std::optional<DiffTest>();
  task_difftest(sim_base, diff_ref, image_name, difftest_en);

  // -----------------------
  // Itrace
  // -----------------------

  auto itrace = std::optional<Itrace>();
  task_itrace(sim_base, itrace, itrace_log_en);

  // -----------------------
  // SimJtag(remote bitbang)
  // -----------------------

  auto rbb_simjtag = std::optional<RemoteBitBang>();
  task_simjtag(rbb_en, rbb_port, sim_base, rbb_simjtag);

  // --------------------------
  // Simulator Start Excuting
  // --------------------------

  if (wave_en) {
    const auto wave_name = "wave.fst";
    sim_base.enable_wave_trace(wave_name, wave_stime);
    console->info("Wave init finished, File:{}", wave_name);
  }
  if (corotinue_en) {
    sim_base.enable_corotinue();
  }
  sim_mem.load_file(image_name.c_str());

  auto start_time = std::chrono::utc_clock::now();

  sim_base.prepare();
  // simulator loop
  while (!sim_base.finished() && !is_exit && sim_base.cycle_num < max_cycles) {
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
  bool return_code = !(success && sim_base.exit_normal());


  if (return_code) {
    console->critical("Simulator exit with error");
  } else {
    console->info("Simulator exit with success");
  }


  // close the file
  // fclose(err_file);
  return return_code;
}
