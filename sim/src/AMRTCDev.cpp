#include "include/AMRTCDev.h"
#include "include/Utils.h"
#include <iostream>
#include <chrono>
//pub const DEVICE_BASE: u64 = 0xa0000000;
//pub const SERIAL_PORT: u64 = DEVICE_BASE + 0x00003f8;
//pub const RTC_ADDR: u64 = DEVICE_BASE + 0x0000048;
namespace SimDevices {


    AMRTCDev::AMRTCDev(uint64_t base_addr, uint64_t addr_lenth) {
        this->base_addr = base_addr;
        this->addr_lenth = addr_lenth;
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
            auto offset = read_addr - base_addr;

            if (offset == 0) {
                auto milliseconds_since_epoch = std::chrono::duration_cast<std::chrono::microseconds>(
                        std::chrono::system_clock::now().time_since_epoch()).count();
                rtc_time = milliseconds_since_epoch;
           last_read = rtc_time & 0xffffffffl;
            } else if (offset == 4) {
                last_read = (rtc_time >> 32) & 0xffffffffl;
            } else {
                MY_ASSERT(false, "read address out of range");
            }
        }
        return last_read;
    }
}