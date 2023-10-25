
#include "include/DeviceMange.h"
#include "Utils.h"
#include <ranges>

namespace SimDevices {
    void DeviceMange::add_device(DeviceBase *device) {
        device_pool.push_back(device);
    }

    void DeviceMange::update_inputs(
            uint64_t read_addr,
            bool read_en,
            WriteReq write_req,
            bool write_en
    ) {
        auto rdevice = std::ranges::find_if(device_pool, [&read_addr](auto item) {
            return item->in_range(read_addr);
        });

        auto wdevice = std::ranges::find_if(device_pool, [&write_req](auto item) {
            return item->in_range(write_req.waddr);
        });

        if (rdevice != device_pool.end() && read_en) {
            (*rdevice)->update_inputs(read_addr, read_en, write_req, false);
        }
        if (wdevice != device_pool.end() && write_en) {
            (*wdevice)->update_inputs(read_addr, false, write_req, write_en);
        }
    }

    uint64_t DeviceMange::update_outputs() {
        static uint64_t last_read = 0;
        for (auto device: device_pool) {
            if (!device->read_req_seq.empty()) {
                last_read = device->update_outputs();
            }
            if (!device->write_req_seq.empty()) {
                device->update_outputs();
            }
        }
        return last_read;
    }
}

