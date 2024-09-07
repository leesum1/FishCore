#include "AllTask.h"

void task_am_ebreak_check(SimBase &sim_base, bool am_en) {
  auto console = spdlog::get("console");
  if (am_en) {
    sim_base.add_after_clk_rise_task(
        {[&sim_base, console] {
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
}