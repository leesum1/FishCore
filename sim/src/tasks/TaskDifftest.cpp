#include "AllTask.h"
#include "spdlog/logger.h"
#include <cstdio>
#include <memory>

constexpr auto MEM_BASE = 0x80000000L;
constexpr auto MEM_SIZE = 0x8000000L;
constexpr auto DEVICE_BASE = 0xa0000000L;
constexpr auto SERIAL_PORT = DEVICE_BASE + 0x00003f8L;
constexpr auto RTC_ADDR = DEVICE_BASE + 0x0000048L;
constexpr auto KBD_ADDR = DEVICE_BASE + 0x0000060L;
constexpr auto VGACTL_ADDR = DEVICE_BASE + 0x0000100L;
constexpr auto FB_ADDR = DEVICE_BASE + 0x1000000L;
constexpr auto BOOT_PC = 0x80000000L;

static std::shared_ptr<spdlog::logger> diff_trace = nullptr;
static std::shared_ptr<spdlog::logger> console = nullptr;

// TODO: 有性能问题
void task_difftest(SimBase &sim_base, std::optional<DiffTest> &diff_ref,
                   std::string image_name, bool difftest_en) {

  if (difftest_en) {
    diff_trace = spdlog::get("diff_trace");
    console = spdlog::get("console");

    diff_ref.emplace(BOOT_PC, MEM_SIZE, MEM_BASE);
    diff_ref->load_file(image_name.c_str());
    sim_base.add_after_clk_rise_task(
        {.task_func =
             [&sim_base, &diff_ref] {
               const auto top = sim_base.top;
               // diff test
               if (top->io_difftest_valid) {
                 const auto step_num = top->io_difftest_bits_commited_num;
                 const auto has_exception =
                     top->io_difftest_bits_exception_valid;
                 const auto has_interrupt = top->io_difftest_bits_has_interrupt;
                 const auto cause = top->io_difftest_bits_exception_cause;
                 const auto has_mmio = top->io_difftest_bits_contain_mmio;
                 const auto has_csr_skip = top->io_difftest_bits_csr_skip;
                 const auto is_rvc = top->io_difftest_bits_last_is_rvc;
                 const auto pc = top->io_difftest_bits_last_pc;
                 const auto next_pc = pc + (is_rvc ? 2 : 4); // for diff skip

                 if (has_interrupt & has_exception ||
                     has_exception & has_mmio || has_exception & has_csr_skip ||
                     has_mmio & has_csr_skip || has_interrupt & has_mmio ||
                     has_interrupt & has_csr_skip) {
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
                   const bool pc_mismatch = diff_ref->check_pc(
                       diff_ref->get_pc(), sim_base.get_pc());
                   const bool gpr_mismatch = diff_ref->check_gprs(
                       [&](const size_t idx) { return sim_base.get_reg(idx); },
                       [&](const size_t idx) {
                         return diff_ref->get_reg(idx);
                       });
                   const bool csr_mismatch = diff_ref->check_csrs(
                       [&](const size_t idx) { return sim_base.get_csr(idx); });
                   const bool mismatch =
                       pc_mismatch | gpr_mismatch | csr_mismatch;

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
         .name = "difftest",
         .period_cycle = 0,
         .type = SimTaskType::period});
  }
}