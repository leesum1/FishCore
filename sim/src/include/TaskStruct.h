#pragma once

#include "async_simple/coro/Lazy.h"

enum class SimTaskType { once, period };

struct SimTask_t {
  std::function<void()> task_func;
  std::string name;

  const uint64_t period_cycle;
  SimTaskType type = SimTaskType::period;
  uint64_t counter = 0;

  void run_sync() {
    counter += 1;
    if (counter >= period_cycle) {
      counter = 0;
      task_func();
    }
  }

  async_simple::coro::Lazy<void> task_func_co_wrapper() {
    task_func();
    co_return;
  }

  async_simple::coro::Lazy<void> run_co() {
    counter += 1;
    if (counter >= period_cycle) {
      counter = 0;
      co_await task_func_co_wrapper();
    }
    co_return;
  }
};