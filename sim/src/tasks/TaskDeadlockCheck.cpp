#include "AllTask.h"

static std::shared_ptr<spdlog::logger> console = nullptr;



void task_deadlock_check(SimBase &sim_base) {
  console = spdlog::get("console");
  sim_base.add_after_clk_rise_task(
      {[&sim_base] {
         if (sim_base.not_commit_num > 4096) {
           if (sim_base.top->io_is_halted != 0) {
             // not check on halt
             sim_base.not_commit_num = 0;
           } else {
             console->critical(
                 "no commit for {} cycles, dead lock at pc: 0x{:016x}\n", 4096,
                 sim_base.get_pc());
             sim_base.set_state(SimBase::sim_abort);
           }
         }
       },
       "dead_lock_check", 4096});
}