#pragma once

#include "DeviceBase.h"

namespace SimDevices {
    class DeviceMange {
        std::vector<DeviceBase *> device_pool;

        bool is_conflit(uint64_t start, uint64_t end);

    public:
        void add_device(DeviceBase *device);

        void print_device_info();

        uint64_t update_outputs();

        void update_inputs(
                uint64_t read_addr,
                bool read_en,
                WriteReq write_req,
                bool write_en
        );
    };
}

