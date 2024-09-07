#pragma once

#include "DeviceBase.h"

namespace SimDevices {
class DeviceMange {
  std::vector<DeviceBase *> device_pool;

  bool is_conflict(uint64_t start, uint64_t end) const;

public:
  void add_device(DeviceBase *device);

  void print_device_info() const;

  uint64_t update_outputs() const;

  bool update_inputs(uint64_t read_addr, bool read_en, WriteReq write_req,
                     bool write_en);
};
} // namespace SimDevices
