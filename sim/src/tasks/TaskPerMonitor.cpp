#include "AllTask.h"

void task_perfmonitor(SimBase &sim_base, PerfMonitor &perf_monitor,
                      bool perf_trace_log_en) {
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
        {.task_func = [&] { perf_monitor.print_perf_counter(false); },
         .name = "perf_monitor",
         .period_cycle = 8192,
         .type = SimTaskType::period});
  }
}