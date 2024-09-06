#pragma once

#include <arpa/inet.h>
#include <cstdio>
#include <cstring>
#include <fcntl.h>
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
  RemoteBitBang(int port);

  ~RemoteBitBang();

  bool try_accept();

  void send(char data);

  bool try_recv(char *data);

  void tick(unsigned char *jtag_tck, unsigned char *jtag_tms,
            unsigned char *jtag_tdi, const unsigned char jtag_tdo);

  void execute_command(const char cmd, const unsigned char new_tdo);

  void set_pins(char new_tck, char new_tms, char new_tdi);

  void do_reset();
  void do_quit();

private:
  void setup_server();

  int server_fd;
  int client_fd;
  int port;

  unsigned char tclk;
  unsigned char tms;
  unsigned char tdi;

  std::optional<unsigned char> tdo;
  char trst;
};