#include "AllTask.h"
#include <cstdint>
#include <cstdlib>

static std::shared_ptr<spdlog::logger> console = nullptr;

void task_tohost_check(SimBase &sim_base, SimDevices::SynReadMemoryDev &sim_mem,
                       bool to_host_check_enabled) {

  static auto tohost_callback_func = [&sim_mem,
                                      &sim_base](uint64_t tohost_data) {
    sim_mem.check_to_host([&](const uint64_t to_host_data) {
      sim_base.set_state(SimBase::sim_stop);

      console->info("Write tohost {:x} at pc {:x}", to_host_data,
                    sim_base.get_pc());

      // TODO: add more tohost check
    });
  };

  if (to_host_check_enabled) {
    console = spdlog::get("console");

    sim_base.add_once_time_task(
        {.task_func =
             [&sim_base, &sim_mem] {
               auto top = sim_base.top;
               if (sim_mem.get_to_host_addr().has_value()) {
                 top->io_tohost_addr_bits = sim_mem.get_to_host_addr().value();
                 top->io_tohost_addr_valid = true;
                 console->info("Set to_host_addr: 0x{:016x}\n",
                               sim_mem.get_to_host_addr().value());
               } else {
                 top->io_tohost_addr_valid = false;
                 top->io_tohost_addr_bits = 0;
                 console->info("to_host_addr not found\n");
                 exit(-1);
               }
             },
         .name = "Set to_host_addr",
         .period_cycle = 0,
         .type = SimTaskType::once});

    // for riscof and riscv-tests, use to_host to communicate with simulation
    // environment
    sim_base.add_after_clk_rise_task({.task_func =
                                          [&sim_mem, &sim_base] {
                                            sim_mem.check_to_host(
                                                tohost_callback_func);
                                          },
                                      .name = "to_host_check",
                                      .period_cycle = 1024,
                                      .type = SimTaskType::period});
  }
}