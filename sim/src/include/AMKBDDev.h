#pragma once

#include "DeviceBase.h"
#include <SDL2/SDL.h>
#include <readerwriterqueue.h>

namespace SimDevices {
class AMKBDDev final : public DeviceBase {
  uint64_t rtc_time = 0;
  uint64_t mem_addr;
  uint64_t mem_size;

  /* 提取自 native 的键盘映关系,cpp 中不支持数组乱序初始化,无奈之举 */
  int keymap[256] = {
      0,  0,  0,  0,  43, 60, 58, 45, 31, 46, 47, 48, 36, 49, 50, 51, 62, 61,
      37, 38, 29, 32, 44, 33, 35, 59, 30, 57, 34, 56, 15, 16, 17, 18, 19, 20,
      21, 22, 23, 24, 54, 1,  27, 28, 70, 25, 26, 39, 40, 41, 0,  52, 53, 14,
      63, 64, 65, 42, 2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 0,  0,
      0,  77, 79, 81, 78, 80, 82, 76, 75, 74, 73, 0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  68, 0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,  67, 55, 69, 0,  72, 66, 71, 0,  0,  0,
      0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
      0,  0,  0,  0};

  moodycamel::ReaderWriterQueue<int> scancode_queue;
  moodycamel::ReaderWriterQueue<SDL_Keycode> ascii_queue;

public:
  explicit AMKBDDev(uint64_t base_addr);

  void create_kdb_thread();

  void update_inputs(uint64_t read_addr, bool read_en, WriteReq write_req,
                     bool write_en) override;

  uint64_t update_outputs() override;

  bool in_range(uint64_t addr) override;

  std::vector<AddrInfo> get_addr_info() override;
};
} // namespace SimDevices
