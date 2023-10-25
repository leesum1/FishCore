#pragma once


#include "DeviceBase.h"

namespace SimDevices {

    class AMUartDev : public DeviceBase {

        void update_inputs(
                uint64_t read_addr,
                bool read_en,
                WriteReq write_req,
                bool write_en
        );

        uint64_t update_outputs();

    public:
        AMUartDev(uint64_t base_addr, uint64_t addr_lenth);
    };
}


