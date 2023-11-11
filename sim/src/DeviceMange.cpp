#include "include/DeviceMange.h"
#include "Utils.h"
#include <format>
#include <iostream>

namespace SimDevices
{
    void DeviceMange::add_device(DeviceBase* device)
    {
        bool conflict = false;
        auto addr_info = device->get_addr_info();
        for (auto info : addr_info)
        {
            if (is_conflit(info.start, info.end))
            {
                conflict = true;
                break;
            }
        }

        MY_ASSERT(conflict == false, "device: {} address conflict", device->get_addr_info()[0].name);

        device_pool.push_back(device);
    }

    void DeviceMange::update_inputs(
        uint64_t read_addr,
        bool read_en,
        WriteReq write_req,
        bool write_en
    )
    {
        auto rdevice = std::ranges::find_if(device_pool, [&read_addr](auto item)
        {
            return item->in_range(read_addr);
        });

        auto wdevice = std::ranges::find_if(device_pool, [&write_req](auto item)
        {
            return item->in_range(write_req.waddr);
        });

        if (read_en)
        {
            if (rdevice != device_pool.end())
            {
                (*rdevice)->update_inputs(read_addr, read_en, write_req, false);
            }
            else
            {
                std::cout << std::format("read address: {:#010X}\n", read_addr);
                MY_ASSERT(false, "read address out of range");
            }
        }

        if (write_en)
        {
            if (wdevice != device_pool.end())
            {
                (*wdevice)->update_inputs(read_addr, false, write_req, write_en);
            }
            else
            {
                std::cout << std::format("write address: {:#010X}\n", write_req.waddr);
                MY_ASSERT(false, "write address out of range");
            }
        }
    }

    uint64_t DeviceMange::update_outputs()
    {
        static uint64_t last_read = 0;
        for (auto device : device_pool)
        {
            if (!device->read_req_seq.empty())
            {
                last_read = device->update_outputs();
            }
            if (!device->write_req_seq.empty())
            {
                device->update_outputs();
            }
        }
        return last_read;
    }


    void DeviceMange::print_device_info()
    {
        std::cout << "Device Info:\n";
        for (auto device : device_pool)
        {
            auto addr_info = device->get_addr_info();
            for (auto [start, end, name] : addr_info)
            {
                std::cout << std::format("device: {:<15} {:#010X} ----> {:#010X}\n",
                                         name,
                                         start,
                                         end);
            }
        }
        std::cout << "---------------------------------------------\n";
    }

    bool DeviceMange::is_conflit(uint64_t start, uint64_t end)
    {
        for (auto device : device_pool)
        {
            for (auto addr_info = device->get_addr_info(); const auto& info : addr_info)
            {
                // Check if there is any overlap between [start, end) and [info.start, info.end)
                if (start < info.end && end > info.start)
                {
                    return true;
                }
            }
        }
        return false;
    }
}
