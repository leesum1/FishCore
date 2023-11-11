

#include <iostream>
#include <readerwriterqueue.h>
#include <thread>
#include <SDL.h>
#include "include/AMKBDDev.h"
#include "include/Utils.h"


namespace SimDevices {
    AMKBDDev::AMKBDDev(uint64_t base_addr) {
        mem_addr = base_addr;
        mem_size = 8;
        scancode_queue = moodycamel::ReaderWriterQueue<int>(128);
        ascii_queue = moodycamel::ReaderWriterQueue<SDL_Keycode>(128);

        std::cout << std::format("am_keyboard at 0x{:x}\n", mem_addr);


    }

    void AMKBDDev::update_inputs(
            uint64_t read_addr,
            bool read_en,
            WriteReq write_req,
            bool write_en
    ) {
        if (read_en) {
            MY_ASSERT(in_range(read_addr), "read address out of range");
            read_req_seq.push_back(read_addr);
        }
        if (write_en) {
            MY_ASSERT(false, "write not supported");
        }
    }

    uint64_t AMKBDDev::update_outputs() {
        MY_ASSERT(write_req_seq.empty());

        if (!read_req_seq.empty()) {
            auto read_addr = read_req_seq.back();
            read_req_seq.pop_back();
            auto offset = read_addr - mem_addr;

            MY_ASSERT(offset == 0 || offset == 4, "read offset not supported");
            bool succeeded = false;
            if (offset == 0) {
                int am_code;
                succeeded = scancode_queue.try_dequeue(am_code);
                last_read = succeeded ? am_code : 0;
            } else if (offset == 4) {
                int sdl_code;
                succeeded = ascii_queue.try_dequeue(sdl_code);
                last_read = succeeded ? sdl_code : 0;
            }
        }
        return last_read;
    }

    bool AMKBDDev::in_range(uint64_t addr) {
        return addr >= mem_addr && addr < mem_addr + mem_size;
    }

    std::vector<AddrInfo> AMKBDDev::get_addr_info() {
        return {
                {mem_addr, mem_addr + mem_size, "am_keyboard"}
        };
    }

    void AMKBDDev::create_kdb_thread() {
        std::thread writer([&]() {
            auto send_am_key = [&](uint8_t scancode, bool is_keydown) {
                if (keymap[scancode] != 0) {
#define KEYDOWN_MASK 0x8000
                    int am_code = keymap[scancode] | (is_keydown ? KEYDOWN_MASK : 0);
                    scancode_queue.enqueue(am_code);
                }
            };

            auto send_ascii_key = [&](SDL_Keycode kcode) {
                // sdl keycode 包含 ascii 以外的内容
                if (kcode <= UINT8_MAX) {
                    ascii_queue.enqueue(kcode);
                }
            };
            SDL_Event event;
            while (true) {
                while (SDL_PollEvent(&event)) {
                    switch (event.type) {
                        case SDL_QUIT:
                            break;
                            // If a key was pressed
                        case SDL_KEYDOWN:
                        case SDL_KEYUP: {
                            SDL_Scancode k = event.key.keysym.scancode;
                            bool is_keydown = (event.key.type == SDL_KEYDOWN);
                            send_am_key(k, is_keydown);

                            if (event.type == SDL_KEYDOWN) {
                                SDL_Keycode k_ascii = SDL_GetKeyFromScancode(k);
                                // shift - 组合键 转换为 _
                                if ((event.key.keysym.mod & KMOD_SHIFT) && k_ascii == SDLK_MINUS) {
                                    send_ascii_key(SDL_KeyCode::SDLK_UNDERSCORE);
                                } else {
                                    send_ascii_key(k_ascii);
                                }
                            }

                            break;
                        }
                        default:
                            break;
                    }
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
            }
        });
        writer.detach();
    }
}