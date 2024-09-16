//
// Created by leesum on 10/23/23.
//

#include "include/SramMemoryDev.h"
#include "include/Utils.h"
#include <bit>
#include <cstdint>
#include <format>
#include <functional>
#include <iostream>
#include <optional>

namespace SimDevices {
SynReadMemoryDev::SynReadMemoryDev(uint64_t base_addr, uint32_t mem_size) {
  this->mem_addr = base_addr;
  this->mem_size = mem_size;
  mem = std::vector<uint8_t>(mem_size);
  MY_ASSERT(mem.size() == mem_size, "memory size not match");
}

uint64_t SynReadMemoryDev::read(uint64_t addr) {
  //        if (in_range(addr)) {
  //            return 0;
  //        }
  MY_ASSERT(in_range(addr), "read address out of range");
  MY_ASSERT(Utils::check_aligned(addr, 8), "read address not aligned");
  uint64_t result = 0;
  std::memcpy(&result, &mem[addr - mem_addr], sizeof(uint64_t));
  return result;
}

void SynReadMemoryDev::write(uint64_t addr, uint64_t wdata, uint8_t wstrb) {
  MY_ASSERT(in_range(addr), "write address out of range");
  MY_ASSERT(Utils::check_aligned(addr, 8), "write address not aligned");
  auto wdata_seq = std::bit_cast<std::array<uint8_t, 8>>(wdata);

  for (int i = 0; i < 8; i++) {
    if (wstrb & 1 << i) {
      mem[addr - mem_addr + i] = wdata_seq[i];
    }
  }
}

void SynReadMemoryDev::update_inputs(uint64_t read_addr, bool read_en,
                                     WriteReq write_req, bool write_en) {
  if (read_en) {
    read_req_seq.emplace_back(read_addr);
  }
  if (write_en) {
    write_req_seq.emplace_back(write_req);
  }

  if (read_en && write_en) {
    //        MY_ASSERT(aligned_addr(read_addr) !=
    //        aligned_addr(write_req_seq.waddr), "read and write conflict");
  }
}

/**
 * @brief First read, then write,if read write the same address, the read will
 * get the old value
 * @return
 */
uint64_t SynReadMemoryDev::update_outputs() {
  if (!read_req_seq.empty()) {
    auto read_addr = read_req_seq.back();
    read_req_seq.pop_back();

    last_read = read(Utils::aligned_addr(read_addr));
  }
  if (!write_req_seq.empty()) {
    auto write_req = this->write_req_seq.back();
    this->write_req_seq.pop_back();
    write(Utils::aligned_addr(write_req.waddr), write_req.wdata,
          write_req.wstrb);
  }
  return last_read;
}

bool SynReadMemoryDev::load_elf(const char *file_name) {
  using namespace ELFIO;
  // Create elfio reader
  auto reader = elfio();

  // Load ELF data
  if (!reader.load(file_name)) {
    std::cout << "Can't find or process ELF file " << file_name << std::endl;
    return false;
  }

  MY_ASSERT(reader.get_class() == ELFCLASS64, "elf class not match");
  MY_ASSERT(reader.get_encoding() == ELFDATA2LSB, "elf data not match");
  MY_ASSERT(reader.get_machine() == EM_RISCV, "elf machine not match");

  load_elf_to_mem(reader);
  collect_elf_symbols(reader);

  std::cout << "Loading elf file " << file_name << std::endl;

  //        // print symbols
  //        for (const auto &item: elf_symbol_map) {
  //            std::cout << std::format("symbol {} value: 0x{:x}\n",
  //            item.first, item.second);
  //        }
  auto to_host_find = elf_symbol_map.find("tohost");

  if (to_host_find != elf_symbol_map.end()) {
    to_host_addr = to_host_find->second;
  }

  return true;
}

void SynReadMemoryDev::load_elf_to_mem(ELFIO::elfio &reader) {
  using namespace ELFIO;
  for (const auto &pseg : reader.segments) {
    if (pseg->get_type() == PT_LOAD) {
      // load segment to memory
      const char *p = pseg->get_data();
      std::memcpy(&this->mem[pseg->get_physical_address() - 0x80000000], p,
                  pseg->get_file_size());
    }
  }
}

void SynReadMemoryDev::collect_elf_symbols(ELFIO::elfio &reader) {
  using namespace ELFIO;
  for (auto &psec : reader.sections) {
    if (psec->get_type() == SHT_SYMTAB) {
      const symbol_section_accessor symbols(reader, psec.get());
      for (unsigned int j = 0; j < symbols.get_symbols_num(); ++j) {
        std::string name;
        Elf64_Addr value;
        Elf_Xword size;
        unsigned char bind;
        unsigned char type;
        Elf_Half section_index;
        unsigned char other;
        symbols.get_symbol(j, name, value, size, bind, type, section_index,
                           other);
        // record symbol
        this->elf_symbol_map[name] = value;
      }
    }
  }
}

/**
 * @brief load file to memory, if the file is elf, load elf to memory, else load
 * the file to memory
 * @param file_name
 */
void SynReadMemoryDev::load_file(const char *file_name) {
  if (!load_elf(file_name)) {
    std::ifstream file(file_name, std::ios::binary);
    if (!file.is_open()) {
      std::cout << "Error: could not open file " << file_name << std::endl;
      exit(1);
    }

    std::cout << "Loading file " << file_name << std::endl;

    file.read(reinterpret_cast<char *>(mem.data()), mem.size());
    file.close();
  }
}

void SynReadMemoryDev::dump_signature(std::string_view signature_file_name) {
  const auto sig_start = elf_symbol_map.find("begin_signature");
  const auto sig_end = elf_symbol_map.find("end_signature");

  if (sig_start != this->elf_symbol_map.end() &&
      sig_end != this->elf_symbol_map.end()) {
    std::ofstream signature_file(signature_file_name.data(), std::ios::binary);
    if (!signature_file.is_open()) {
      std::cout << "Error: could not open file " << signature_file_name
                << std::endl;
      return;
    }

    for (auto i = sig_start->second; i < sig_end->second; i += 4) {
      uint32_t value = *reinterpret_cast<uint32_t *>(&mem[i - 0x80000000]);

      auto fmt = std::format("{:08x}\n", value);
      signature_file.write(fmt.data(), fmt.size());
    }

    signature_file.close();

    std::cout << std::format(
        "dump signature to {},sig_start: 0x{:x}, sig_end: 0x{:x}\n",
        signature_file_name, sig_start->second, sig_end->second);
  }
}

void SynReadMemoryDev::check_to_host(
    const std::function<void(uint64_t)> &tohost_callback) {

  if (!to_host_addr.has_value()) {
    return;
  }

  if (uint64_t to_host_value = read(to_host_addr.value()); to_host_value != 0) {
    tohost_callback(to_host_value);
    // clear after read
    write(to_host_addr.value(), 0, 0xff);
  }
}

std::optional<uint64_t> SynReadMemoryDev::get_to_host_addr() {
  return to_host_addr;
}

bool SynReadMemoryDev::in_range(uint64_t addr) {
  return addr >= mem_addr && addr < mem_addr + mem_size;
}

std::vector<AddrInfo> SynReadMemoryDev::get_addr_info() {
  return {
      {mem_addr, mem_addr + mem_size, "syn_read_mem"},
  };
}
} // namespace SimDevices
