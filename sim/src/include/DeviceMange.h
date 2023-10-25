#pragma once

#include "DeviceBase.h"

namespace SimDevices {
    class DeviceMange {
        std::vector<DeviceBase *> device_pool;

    public:
        void add_device(DeviceBase *device);

        uint64_t update_outputs();

        void update_inputs(
                uint64_t read_addr,
                bool read_en,
                WriteReq write_req,
                bool write_en
        );
    };
}

