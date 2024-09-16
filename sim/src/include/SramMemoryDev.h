#pragma once

#include "DeviceBase.h"
#include "elfio/elfio.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>

namespace SimDevices {
class SynReadMemoryDev final : public DeviceBase {
  std::vector<uint8_t> mem;
  std::unordered_map<std::string, uint64_t> elf_symbol_map;
  uint64_t mem_addr;
  uint64_t mem_size;
  std::optional<uint64_t> to_host_addr;

  bool load_elf(const char *file_name);
  void collect_elf_symbols(ELFIO::elfio &reader);
  void load_elf_to_mem(ELFIO::elfio &reader);

public:
  explicit SynReadMemoryDev(uint64_t base_addr, uint32_t mem_size);
  void load_file(const char *file_name);
  void dump_signature(std::string_view signature_file_name);
  void check_to_host(const std::function<void(uint64_t)> &exit_callback);
  uint64_t read(uint64_t addr);
  void write(uint64_t addr, uint64_t wdata, uint8_t wstrb);
  uint64_t update_outputs() override;
  void update_inputs(uint64_t read_addr, bool read_en, WriteReq write_req,
                     bool write_en) override;
  bool in_range(uint64_t addr) override;
  std::optional<uint64_t> get_to_host_addr();

  std::vector<AddrInfo> get_addr_info() override;
  ~SynReadMemoryDev() override = default;
};
} // namespace SimDevices
