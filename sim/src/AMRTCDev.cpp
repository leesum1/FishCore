#include "include/AMRTCDev.h"
#include "include/Utils.h"
#include <iostream>
#include <chrono>
//pub const DEVICE_BASE: u64 = 0xa0000000;
//pub const SERIAL_PORT: u64 = DEVICE_BASE + 0x00003f8;
//pub const RTC_ADDR: u64 = DEVICE_BASE + 0x0000048;
namespace SimDevices {


    AMRTCDev::AMRTCDev(uint64_t base_addr) {
        mem_addr = base_addr;
        mem_size = 8;
    }

    void AMRTCDev::update_inputs(
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

    uint64_t AMRTCDev::update_outputs() {
        MY_ASSERT(write_req_seq.empty());

        if (!read_req_seq.empty()) {
            auto read_addr = read_req_seq.back();
            read_req_seq.pop_back();
            auto offset = read_addr - mem_addr;

            MY_ASSERT(offset == 0 || offset == 4, "read offset not supported");

            if (offset == 0) {
                auto milliseconds_since_epoch = std::chrono::duration_cast<std::chrono::microseconds>(
                        std::chrono::system_clock::now().time_since_epoch()).count();
                rtc_time = milliseconds_since_epoch;
            }
            last_read = rtc_time;
        }
        return last_read;
    }

    bool AMRTCDev::in_range(uint64_t addr) {
        return addr >= mem_addr && addr < mem_addr + mem_size;
    }

    std::vector<AddrInfo> AMRTCDev::get_addr_info() {
        return {
                {mem_addr, mem_addr+mem_size, "am_rtc"}
        };
    }
}