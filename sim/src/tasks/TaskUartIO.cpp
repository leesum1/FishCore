#include "AllTask.h"
#include <cstdio>
#include <fcntl.h>
#include <sys/stat.h>

static std::shared_ptr<spdlog::logger> console_log = nullptr;

// rx pipe
static int pipe_fd[2];

// Function to set a file descriptor to non-blocking mode
static void set_nonblocking(int fd) {
  int flags = fcntl(fd, F_GETFL, 0);
  if (flags == -1) {
    std::cerr << "Failed to get file flags." << std::endl;
    std::exit(EXIT_FAILURE);
  }
  if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) {
    std::cerr << "Failed to set non-blocking mode." << std::endl;
    std::exit(EXIT_FAILURE);
  }
}

static void uart_rx_thread(int pipe_tx_fd) {
  while (true) {
    char c = getchar();
    if (write(pipe_tx_fd, &c, 1) != 1) {
      std::cerr << "Failed to write to pipe." << std::endl;
    }
  }
}

void task_uart_io(SimBase &sim_base) {

  console_log = spdlog::get("console");

  // create pipe
  if (pipe(pipe_fd) == -1) {
    console_log->critical("Failed to create RX pipe.");
    std::exit(EXIT_FAILURE);
  }
  // 设置管道的读端为非阻塞模式
  set_nonblocking(pipe_fd[0]);
  // 创建线程
  console_log->info("UART RX thread started.");
  std::thread rx_thread(uart_rx_thread, pipe_fd[1]);
  rx_thread.detach();

  sim_base.add_after_clk_rise_task({.task_func =
                                        [&] {
                                          const auto top = sim_base.top;

                                          // tx
                                          top->io_uart_tx_deq_ready = 1;
                                          if (top->io_uart_tx_deq_valid) {
                                            char c = static_cast<char>(
                                                top->io_uart_tx_deq_bits);
                                            std::printf("%c", c);
                                          }
                                          // rx, try to read from pipe
                                          top->io_uart_rx_enq_valid = 0;
                                          if (top->io_uart_rx_enq_ready) {
                                            char c;
                                            if (read(pipe_fd[0], &c, 1) == 1) {
                                              top->io_uart_rx_enq_valid = 1;
                                              top->io_uart_rx_enq_bits = c;
                                            }
                                          }
                                        },
                                    .name = "UartFifoTask",
                                    .period_cycle = 0,
                                    .type = SimTaskType::period});
}