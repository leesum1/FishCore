#include "AllTask.h"
#include <cstdio>

void task_uart_io(SimBase &sim_base) {
  sim_base.add_after_clk_rise_task({.task_func =
                                        [&] {
                                          const auto top = sim_base.top;
                                          if (top->io_uart_io_tx_data_valid) {
                                            char c = static_cast<char>(
                                                top->io_uart_io_tx_data_bits);
                                            std::printf("%c", c);
                                          }
                                        },
                                    .name = "uart io",
                                    .period_cycle = 0,
                                    .type = SimTaskType::period});
}