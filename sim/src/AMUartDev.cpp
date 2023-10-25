#include "include/AMUartDev.h"
#include "include/Utils.h"
#include <iostream>
//pub const DEVICE_BASE: u64 = 0xa0000000;
//pub const SERIAL_PORT: u64 = DEVICE_BASE + 0x00003f8;
//pub const RTC_ADDR: u64 = DEVICE_BASE + 0x0000048;
namespace SimDevices {


    AMUartDev::AMUartDev(uint64_t base_addr, uint64_t addr_lenth) {
        this->base_addr = base_addr;
        this->addr_lenth = addr_lenth;
    }

    void AMUartDev::update_inputs(
            uint64_t read_addr,
            bool read_en,
            WriteReq write_req,
            bool write_en
    ) {
        if (read_en) {
            MY_ASSERT(false, "read not supported");
        }
        if (write_en) {
            MY_ASSERT(in_range(write_req.waddr), "write address out of range");
            write_req_seq.push_back(write_req);
        }
    }

    uint64_t AMUartDev::update_outputs() {
        MY_ASSERT(read_req_seq.empty());
        if (!write_req_seq.empty()) {
            auto write_req = write_req_seq.back();
            write_req_seq.pop_back();
            auto offset = write_req.waddr - base_addr;
            MY_ASSERT(offset == 0);
            char c = static_cast<char>(write_req.wdata & 0xff);
            std::cout << c;
        }
        
        uint64_t ret = 0;
        return ret;
    }
}