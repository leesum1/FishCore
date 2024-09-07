#pragma once

#include "DeviceBase.h"
#include "SDL_stdinc.h"
#include <SDL2/SDL.h>
#include <cstdint>
#include <string_view>

namespace SimDevices {
class AMVGADev final : public DeviceBase {
  SDL_Window *window = nullptr;
  SDL_Renderer *renderer = nullptr;
  SDL_Texture *texture = nullptr;
  uint8_t *fbbuff = nullptr;
  uint64_t vga_ctrl_reg = 0;

  uint64_t fb_addr_start = 0;
  uint64_t fb_addr_lenth = get_fb_size();
  uint64_t ctrl_addr_start = 0;
  uint64_t ctrl_addr_lenth = 8;

  static constexpr uint64_t get_witdh() { return 400; }

  static constexpr uint64_t get_height() { return 300; }

  static constexpr uint64_t get_fb_size() {
    return get_witdh() * get_height() * sizeof(uint32_t);
  }

  void update_screen();

  uint64_t read(uint64_t addr);

  void write(uint64_t addr, uint64_t wdata, uint8_t wstrb);

public:
  void init_screen(std::string_view name);

  AMVGADev(uint64_t fb_addr_start, uint64_t ctrl_addr_start);

  ~AMVGADev() override;

  void update_inputs(uint64_t read_addr, bool read_en, WriteReq write_req,
                     bool write_en) override;

  uint64_t update_outputs() override;

  bool in_range(uint64_t addr) override;

  std::vector<AddrInfo> get_addr_info() override;
};
} // namespace SimDevices
