#pragma once

#include "capstone.h"

#include "spdlog/spdlog.h"

class Itrace {
  std::shared_ptr<spdlog::logger> logger;
  std::shared_ptr<spdlog::logger> itrace_log;
  csh handle{};

public:
  Itrace();
  ~Itrace();
  void riscv_disasm(uint32_t code, uint64_t pc);
};
