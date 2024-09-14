#include "AllTask.h"

static std::shared_ptr<spdlog::logger> console = nullptr;

void task_tohost_check(SimBase &sim_base, SimDevices::SynReadMemoryDev &sim_mem,
                       bool to_host_check_enabled) {

  console = spdlog::get("console");

  if (to_host_check_enabled) {
    // for riscof and riscv-tests, use to_host to communicate with simulation
    // environment
    sim_base.add_after_clk_rise_task({[&sim_mem, &sim_base] {
                                        sim_mem.check_to_host([&] {
                                          sim_base.set_state(SimBase::sim_stop);
                                          console->info(
                                              "Write tohost at pc: 0x{:016x}\n",
                                              sim_base.get_pc());
                                        });
                                      },
                                      "to_host_check", 1024});
  }
}