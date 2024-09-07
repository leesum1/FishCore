#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace SimDevices {
struct WriteReq {
  uint64_t waddr;
  uint64_t wdata;
  uint8_t wstrb;
};

struct AddrInfo {
  uint64_t start;
  uint64_t end;
  std::string name;
};

class DeviceBase {

public:
  uint64_t last_read = 0;
  std::vector<uint64_t> read_req_seq;
  std::vector<WriteReq> write_req_seq;

  virtual bool in_range(uint64_t addr) = 0;

  virtual std::vector<AddrInfo> get_addr_info() = 0;

  virtual void update_inputs(uint64_t read_addr, bool read_en,
                             WriteReq write_req, bool write_en) = 0;

  virtual uint64_t update_outputs() = 0;

  virtual ~DeviceBase() = default;
};

} // namespace SimDevices
