#pragma once

#include <arpa/inet.h>
#include <cstdio>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <optional>
#include <unistd.h>

// RemoteBitBang 拥有以下几个功能
// 1. 维护一个 tcp server，服务器的端口可以进行配置, 并且同时只能有一个 client
// 连接
// 2. 实现一个发送函数 send(char data) 和一个接收函数 try_recv(char *data),
// 用于与 client 通信
//    发送函数是阻塞的, 直到 client 确认接收到数据, 接受函数是非阻塞的,
//    如果没有数据, 返回 false
// 3. 实现一个 try_accept 函数(非阻塞), 用于尝试接受一个 client 的连接,
// 如果成功返回 true, 否则返回 false
// 4. 实现一个 tick 函数, 如果有 client 连接,则按以下顺序执行, 如果没有 client
// 连接, 则调用 try_accept
//    1. 调用 try_recv 函数, 如果有数据, 则打印
//    2. 调用 send 函数, 发送一个数据 C
//    3. 退出 tick

class RemoteBitBang {
public:
  RemoteBitBang(int port)
      : server_fd(-1), client_fd(-1), tclk(0), trst(0), tdi(0),
        tdo(std::nullopt), port(port) {
    setup_server();
  }

  ~RemoteBitBang() {
    if (client_fd != -1) {
      close(client_fd);
    }
    if (server_fd != -1) {
      close(server_fd);
    }
  }

  bool try_accept() {
    if (client_fd != -1) {
      return false; // Already has a client connected
    }

    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);
    client_fd = accept(server_fd, (struct sockaddr *)&client_addr, &client_len);
    if (client_fd < 0) {
      return false;
    }
    std::printf("Accepted client connection on port %s:%d\n",
                inet_ntoa(client_addr.sin_addr), ntohs(client_addr.sin_port));
    return true;
  }

  void send(char data) {
    if (client_fd == -1) {
      return;
    }

    // max 100 retries
    // for (int i = 0; i < 100; i++) {
    while (true) {
      ssize_t sent = ::send(client_fd, &data, 1, 0);
      if (sent == 1) {
        // std::cout << "Sent data: " << (int)data << std::endl;
        return;
      } else {
        //   std::cerr << "Error sending data" << std::endl;
        //   do_quit();
      }
      //   usleep(1000);
    }
  }

  bool try_recv(char *data) {
    if (client_fd == -1) {
      return false;
    }

    ssize_t received = recv(client_fd, data, 1, MSG_DONTWAIT);
    return received == 1;
  }

  void tick(unsigned char *jtag_tck, unsigned char *jtag_tms,
            unsigned char *jtag_tdi, const unsigned char jtag_tdo) {
    if (client_fd == -1) {
      try_accept();
      return;
    }

    char cmd;
    if (try_recv(&cmd)) {
      execute_command(cmd, jtag_tdo);
    }
    // std::cout << "Sending data: C" << std::endl;

    if (tdo.has_value()) {
      char send_c = tdo.value() ? '1' : '0';

      send(send_c);
      tdo.reset();
    }

    *jtag_tck = tclk;
    *jtag_tms = tms;
    *jtag_tdi = tdi;
  }

  void execute_command(const char cmd, const unsigned char new_tdo) {
    if (client_fd == -1) {
      return;
    }

    // std::cout << "Received command: " << cmd << std::endl;
    switch (cmd) {
    case '0': {
      set_pins(0, 0, 0);
      break;
    }
    case '1': {
      set_pins(0, 0, 1);
      break;
    }
    case '2': {
      set_pins(0, 1, 0);
      break;
    }
    case '3': {
      set_pins(0, 1, 1);
      break;
    }
    case '4': {
      set_pins(1, 0, 0);
      break;
    }
    case '5': {
      set_pins(1, 0, 1);
      break;
    }
    case '6': {
      set_pins(1, 1, 0);
      break;
    }
    case '7': {
      set_pins(1, 1, 1);
      break;
    }
    case 'R': {
      tdo.emplace(new_tdo);
      break;
    }
    case 'Q': {
      do_quit();
      break;
    }
    case 'r': {
      do_reset();
      break;
    }
    case 'b': {
      break;
    }
    case 'B': {
      break;
    }
    default: {
      std::cerr << "Unknown command: " << cmd << std::endl;
      break;
    }
    }
  }

  void set_pins(char new_tck, char new_tms, char new_tdi) {
    tclk = new_tck;
    tms = new_tms;
    tdi = new_tdi;
  }

  void do_reset() { std::cout << "Resetting" << std::endl; }
  void do_quit() {
    std::cout << "Quitting" << std::endl;
    close(client_fd);
    client_fd = -1;
  }

private:
  void setup_server() {
    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) {
      std::cerr << "Error creating socket" << std::endl;
      return;
    }

    // Set server socket to non-blocking mode
    if (fcntl(server_fd, F_SETFL, O_NONBLOCK) < 0) {
      std::cerr << "Error setting socket to non-blocking" << std::endl;
      close(server_fd);
      server_fd = -1;
      return;
    }

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(port);

    if (bind(server_fd, (struct sockaddr *)&server_addr, sizeof(server_addr)) <
        0) {
      std::cerr << "Error binding socket" << std::endl;
      close(server_fd);
      server_fd = -1;
      return;
    }

    if (listen(server_fd, 1) < 0) {
      std::cerr << "Error listening on socket" << std::endl;
      close(server_fd);
      server_fd = -1;
      return;
    }

    std::cout << "RemoteBitBang  Listening on port " << port << std::endl;
  }

  int server_fd;
  int client_fd;
  int port;

  unsigned char tclk;
  unsigned char tms;
  unsigned char tdi;

  std::optional<unsigned char> tdo;
  char trst;
};