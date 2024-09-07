#include "include/AMVGADev.h"
#include "SDL.h"
#include "SDL_video.h"
#include "include/Utils.h"
#include <array>
#include <bit>
#include <iostream>

namespace SimDevices {
AMVGADev::AMVGADev(uint64_t fb_addr_start, uint64_t ctrl_addr_start)
    : fb_addr_start(fb_addr_start), ctrl_addr_start(ctrl_addr_start) {}

bool AMVGADev::in_range(uint64_t addr) {
  return (addr >= fb_addr_start && addr < fb_addr_start + fb_addr_lenth) ||
         (addr >= ctrl_addr_start && addr < ctrl_addr_start + ctrl_addr_lenth);
}

uint64_t AMVGADev::read(uint64_t addr) {
  // fb buffer area
  if (addr >= fb_addr_start && addr < fb_addr_start + fb_addr_lenth) {
    auto offset = Utils::aligned_addr(addr) - fb_addr_start;
    MY_ASSERT(offset < get_fb_size(), "read address out of range");
    uint64_t read_data = *(uint64_t *)(fbbuff + offset);
    return read_data;
  }
  // control register area
  else if (addr >= ctrl_addr_start &&
           addr < ctrl_addr_start + ctrl_addr_lenth) {
    auto offset = Utils::aligned_addr(addr) - ctrl_addr_start;
    MY_ASSERT(offset < 8, "read address out of range");
    return vga_ctrl_reg;
  } else {
    MY_ASSERT(false, "read address out of range");
    return 0;
  }
}

void AMVGADev::write(uint64_t addr, uint64_t wdata, uint8_t wstrb) {

  auto wdata_seq = std::bit_cast<std::array<uint8_t, 8>>(wdata);
  // fb buffer area
  if (addr >= fb_addr_start && addr < fb_addr_start + fb_addr_lenth) {
    auto offset = Utils::aligned_addr(addr) - fb_addr_start;
    MY_ASSERT(offset < get_fb_size(), "read address out of range");
    for (int i = 0; i < 8; i++) {
      if (wstrb & (1 << i)) {
        fbbuff[offset + i] = wdata_seq[i];
      }
    }
  }
  // control register area
  else if (addr >= ctrl_addr_start &&
           addr < ctrl_addr_start + ctrl_addr_lenth) {
    auto offset = Utils::aligned_addr(addr) - ctrl_addr_start;
    MY_ASSERT(offset < 8, "read address out of range");

    uint64_t mask = 0;
    for (int i = 0; i < 8; i++) {
      if (wstrb & (1 << i)) {
        mask |= static_cast<uint64_t>(0xFF) << (i * 8);
      }
    }
    vga_ctrl_reg = (vga_ctrl_reg & ~mask) | (wdata & mask);
    update_screen();
  } else {
    MY_ASSERT(false, "read address out of range");
  }
}

void AMVGADev::update_inputs(uint64_t read_addr, bool read_en,
                             WriteReq write_req, bool write_en) {
  if (read_en) {
    MY_ASSERT(in_range(read_addr), "read address out of range");
    read_req_seq.push_back(read_addr);
  }
  if (write_en) {
    MY_ASSERT(in_range(write_req.waddr), "write address out of range");
    write_req_seq.push_back(write_req);
  }
}

uint64_t AMVGADev::update_outputs() {
  if (!write_req_seq.empty()) {
    auto write_req = write_req_seq.back();
    write_req_seq.pop_back();
    write(write_req.waddr, write_req.wdata, write_req.wstrb);
  }
  if (!read_req_seq.empty()) {
    auto read_addr = read_req_seq.back();
    read_req_seq.pop_back();
    last_read = read(read_addr);
  }
  return last_read;
}

void AMVGADev::init_screen(std::string_view name) {
  SDL_Init(SDL_INIT_VIDEO);
  SDL_CreateWindowAndRenderer(get_witdh() * 2, get_height() * 2, 0, &window,
                              &renderer);
  SDL_SetWindowTitle(window, name.data());

  texture =
      SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888,
                        SDL_TEXTUREACCESS_STATIC, get_witdh(), get_height());
  fbbuff = new uint8_t[get_fb_size()];
}

void AMVGADev::update_screen() {
  if ((vga_ctrl_reg >> 32) != 0) {
    vga_ctrl_reg &= 0xFFFFFFFFL;
    SDL_UpdateTexture(texture, nullptr, fbbuff, get_witdh() * sizeof(uint32_t));
    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, texture, nullptr, nullptr);
    SDL_RenderPresent(renderer);
  }
}

AMVGADev::~AMVGADev() {
  if (fbbuff != nullptr) {
    delete[] fbbuff;
  }
  if (renderer != nullptr) {
    SDL_DestroyTexture(texture);
  }
  if (texture != nullptr) {
    SDL_DestroyRenderer(renderer);
  }

  if (window != nullptr) {
    SDL_DestroyWindow(window);
  }
  SDL_VideoQuit();
  std::cout << "AMVGADev exit\n";
}

std::vector<AddrInfo> AMVGADev::get_addr_info() {
  return {{fb_addr_start, fb_addr_start + fb_addr_lenth, "am_vga_fb"},
          {ctrl_addr_start, ctrl_addr_start + ctrl_addr_lenth, "am_vga_ctrl"}};
}
} // namespace SimDevices
