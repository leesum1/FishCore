#include "include/DeviceMange.h"
#include "Utils.h"
#include <format>
#include <iostream>

namespace SimDevices {
void DeviceMange::add_device(DeviceBase *device) {
  bool conflict = false;
  for (const auto addr_info = device->get_addr_info();
       const auto &info : addr_info) {
    if (is_conflict(info.start, info.end)) {
      conflict = true;
      break;
    }
  }

  MY_ASSERT(conflict == false, "device: %s address conflict",
            device->get_addr_info()[0].name.c_str());

  device_pool.push_back(device);
}

bool DeviceMange::update_inputs(uint64_t read_addr, const bool read_en,
                                WriteReq write_req, const bool write_en) {
  const auto rdevice =
      std::ranges::find_if(device_pool, [&read_addr](auto item) {
        return item->in_range(read_addr);
      });

  const auto wdevice =
      std::ranges::find_if(device_pool, [&write_req](auto item) {
        return item->in_range(write_req.waddr);
      });

  bool success = true;

  if (read_en) {
    if (rdevice != device_pool.end()) {
      (*rdevice)->update_inputs(read_addr, read_en, write_req, false);
    } else {
      success = false;
      std::cout << std::format("read address out of range: {:#010X}\n",
                               read_addr);
    }
  }

  if (write_en) {
    if (wdevice != device_pool.end()) {
      (*wdevice)->update_inputs(read_addr, false, write_req, write_en);
    } else {
      success = false;
      std::cout << std::format("write address out of range: {:#010X}\n",
                               write_req.waddr);
    }
  }

  return success;
}

uint64_t DeviceMange::update_outputs() const {
  static uint64_t last_read = 0;
  for (const auto device : device_pool) {
    if (!device->read_req_seq.empty()) {
      last_read = device->update_outputs();
    }
    if (!device->write_req_seq.empty()) {
      device->update_outputs();
    }
  }
  return last_read;
}

void DeviceMange::print_device_info() const {
  std::cout << "Device Info:\n";
  for (const auto device : device_pool) {
    for (auto addr_info = device->get_addr_info();
         auto &[start, end, name] : addr_info) {
      std::cout << std::format("device: {:<15} {:#010X} ----> {:#010X}\n", name,
                               start, end);
    }
  }
  std::cout << "---------------------------------------------\n";
}

bool DeviceMange::is_conflict(const uint64_t start, const uint64_t end) const {
  for (const auto device : device_pool) {
    for (auto addr_info = device->get_addr_info();
         const auto &info : addr_info) {
      // Check if there is any overlap between [start, end) and [info.start,
      // info.end)
      if (start < info.end && end > info.start) {
        return true;
      }
    }
  }
  return false;
}
} // namespace SimDevices
