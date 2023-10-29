#pragma once

#include "DeviceBase.h"
#include "SDL_stdinc.h"
#include <SDL2/SDL.h>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <string_view>
#include <sys/types.h>

namespace SimDevices {

    class AMVGADev : public DeviceBase {
        SDL_Window *window = nullptr;
        SDL_Renderer *renderer = nullptr;
        SDL_Texture *texture = nullptr;
        uint8_t *fbbuff = nullptr;
        uint64_t vga_ctrl_reg = 0;
        bool update_en = false;

        uint64_t fb_addr_start = 0;
        uint64_t fb_addr_lenth = get_fb_size();
        uint64_t ctrl_addr_start = 0;
        uint64_t ctrl_addr_lenth = 8;

        constexpr uint64_t get_witdh() { return 400; }

        constexpr uint64_t get_height() { return 300; }

        constexpr uint64_t get_fb_size() {
            return get_witdh() * get_height() * sizeof(uint32_t);
        }

        void update_screen();


        uint64_t read(uint64_t addr);

        void write(uint64_t addr, uint64_t wdata, uint8_t wstrb);

    public:
        bool in_range(uint64_t addr);

        void init_screen(std::string_view name);

        AMVGADev(uint64_t fb_addr_start, uint64_t ctrl_addr_start);

        ~AMVGADev();

        void update_inputs(uint64_t read_addr, bool read_en, WriteReq write_req,
                           bool write_en);

        uint64_t update_outputs();
    };
} // namespace SimDevices

// #ifndef __DEVICEVGA_H_
// #define __DEVICEVGA_H_

// #include "devicebase.h"
// #include <SDL2/SDL.h>
// namespace Topdevice {

//     class Devicevga :public Devicebase {

//     private:
// #define SCREEN_W 400 //默认 400*300
// #define SCREEN_H 300

//         struct vga_reg_t {
//             uint32_t sreensize; //屏幕大小寄存器
//             uint32_t sync;      //同步寄存器
//             uint32_t* fbbuff;   //fb 显存
//         };

//         vga_reg_t vgaregs;
//         SDL_Renderer* renderer = nullptr;
//         SDL_Texture* texture = nullptr;
//         SDL_Thread* update_thread;
//         SDL_mutex* fbbuff_lock;
//     private:
//         void initscreen();
//         uint32_t screen_width();
//         uint32_t screen_height();
//         uint32_t screen_size();
//         void vga_update_screen();

//     public:
//         Devicevga(/* args */);
//         virtual  ~Devicevga();
//         void update_screen();
//         void write(paddr_t addr, word_t data, uint32_t len);
//         word_t read(paddr_t addr);
//         void update();
//         void init(const char* name);
//     };

// } // namespace d

// #endif
