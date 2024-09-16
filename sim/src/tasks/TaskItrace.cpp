
#include "AllTask.h"

static std::shared_ptr<spdlog::logger> itrace_log = nullptr;

void task_itrace(SimBase &sim_base, std::optional<Itrace> &itrace,
                 bool itrace_log_enable) {

  if (itrace_log_enable) {
    itrace_log = spdlog::get("itrace");
    itrace.emplace();
    sim_base.add_after_clk_rise_task(
        {.task_func =
             [&sim_base, &itrace] {
               const auto top = sim_base.top;
               if (top->io_difftest_valid) {
                 const auto has_exception =
                     top->io_difftest_bits_exception_valid;
                 const auto has_interrupt = top->io_difftest_bits_has_interrupt;
                 const auto cause = top->io_difftest_bits_exception_cause;
                 const auto pc = top->io_difftest_bits_last_pc;

                 if (has_exception) {
                   itrace->riscv_disasm(top->io_difftest_bits_inst_info_0_inst,
                                        top->io_difftest_bits_last_pc);
                   itrace_log->info("⬆️ pc 0x{:08x},exception cause 0x{:x}\n",
                                    pc, cause);
                 } else if (has_interrupt) {
                   itrace->riscv_disasm(top->io_difftest_bits_inst_info_0_inst,
                                        top->io_difftest_bits_last_pc);
                   itrace_log->info("⬆️ pc 0x{:08x},interrupt cause 0x{:x}\n",
                                    pc, cause);
                 } else {
                   const auto pc_list = std::array{
                       static_cast<uint64_t>(
                           top->io_difftest_bits_inst_info_0_pc),
                       static_cast<uint64_t>(
                           top->io_difftest_bits_inst_info_1_pc),
                   };
                   const auto inst_list = std::array{
                       static_cast<uint32_t>(
                           top->io_difftest_bits_inst_info_0_inst),
                       static_cast<uint32_t>(
                           top->io_difftest_bits_inst_info_1_inst),
                   };

                   for (int i = 0; i < top->io_difftest_bits_commited_num;
                        i++) {
                     itrace->riscv_disasm(inst_list[i], pc_list[i]);
                   }
                 }
               }
             },
         .name = "Inst Trace",
         .period_cycle = 0,
         .type = SimTaskType::period});
  }
}