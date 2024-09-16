#pragma once

#include "Itrace.h"
#include "PerfMonitor.h"
#include "RemoteBitBang.h"
#include "SimBase.h"
#include "SramMemoryDev.h"
#include "difftest.hpp"
#include "spdlog/spdlog.h"



void task_uart_io(SimBase &sim_base);
void task_perfmonitor(SimBase &sim_base, PerfMonitor &perf_monitor,
                      bool perf_trace_log_en);

void task_tohost_check(SimBase &sim_base, SimDevices::SynReadMemoryDev &sim_mem,
                       bool to_host_check_enabled);
void task_deadlock_check(SimBase &sim_base);
void task_am_ebreak_check(SimBase &sim_base, bool am_en);
void task_difftest(SimBase &sim_base, std::optional<DiffTest> &diff_ref,
                   std::string image_name, bool difftest_en);
void task_itrace(SimBase &sim_base, std::optional<Itrace> &itrace,
                 bool itrace_log_enable);
void task_simjtag(bool rbb_en, int rbb_port, SimBase &sim_base,
                  std::optional<RemoteBitBang> &rbb_simjtag);