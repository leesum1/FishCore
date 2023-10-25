#pragma once


#include "DeviceBase.h"

namespace SimDevices {

    class AMRTCDev : public DeviceBase {

        uint64_t rtc_time = 0;
    public:
        AMRTCDev(uint64_t base_addr, uint64_t addr_lenth);

        void update_inputs(
                uint64_t read_addr,
                bool read_en,
                WriteReq write_req,
                bool write_en
        );

        uint64_t update_outputs();
    };
}


