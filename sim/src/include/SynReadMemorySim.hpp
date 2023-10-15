#pragma once

#include <cstdint>
#include <vector>
#include <fstream>
#include <iostream>
#include "Utils.hpp"
#include <elfio/elfio.hpp>


class SynReadMemorySim {
private:
    struct WriteReq {
        uint64_t waddr;
        uint64_t wdata;
        uint8_t wstrb;
    };
    const uint32_t mem_size;
    std::vector<uint8_t> mem;
    std::unordered_map<std::string, uint64_t> elf_symbol_map;
    uint64_t last_read = 0;

    std::vector<uint64_t> read_req_seq;
    std::vector<WriteReq> write_req_seq;

private:
    bool load_elf(const char *file_name);

    void collect_elf_symbols(ELFIO::elfio &reader);

    void load_elf_to_mem(ELFIO::elfio &reader);


    uint64_t aligned_addr(uint64_t addr) {
        return addr & ~(0x7);
    }

public:
    explicit SynReadMemorySim(uint32_t mem_size);

    void update_inputs(
            uint64_t read_addr,
            bool read_en,
            WriteReq write_req,
            bool write_en
    );

    uint64_t update_outputs();

    void load_file(const char *file_name);

    void dump_signature(std::string_view signature_file_name);

    void check_to_host(const std::function<void(void)> &exit_callback);

    uint64_t read(uint64_t addr);

    void write(uint64_t addr, uint64_t wdata, uint8_t wstrb);

    ~SynReadMemorySim();

};

SynReadMemorySim::SynReadMemorySim(uint32_t mem_size) : mem_size(mem_size) {
    mem = std::vector<uint8_t>(mem_size);
    MY_ASSERT(mem.size() == mem_size);
}

SynReadMemorySim::~SynReadMemorySim() = default;

uint64_t SynReadMemorySim::read(uint64_t addr) {
    if (addr>=mem_size){
        return 0;
    }
    MY_ASSERT(addr < mem_size);
    MY_ASSERT(Utils::check_aligned(addr, 8));
    uint64_t result = 0;
    std::memcpy(&result, &mem[addr], sizeof(uint64_t));
    return result;

}


void SynReadMemorySim::write(uint64_t addr, uint64_t wdata, uint8_t wstrb) {

    MY_ASSERT(addr < mem_size);
    MY_ASSERT(Utils::check_aligned(addr, 8));
    auto wdata_seq = std::bit_cast<std::array<uint8_t, 8>>(wdata);

    for (int i = 0; i < 8; i++) {
        if (wstrb & (1 << i)) {
            mem[addr + i] = wdata_seq[i];
        }
    }
}

void SynReadMemorySim::update_inputs(
        uint64_t read_addr,
        bool read_en,
        WriteReq write_req,
        bool write_en
) {
    if (read_en) {
        read_req_seq.emplace_back(read_addr);
    }
    if (write_en) {
        this->write_req_seq.emplace_back(write_req);
    }

    if (read_en && write_en) {
//        MY_ASSERT(aligned_addr(read_addr) != aligned_addr(write_req_seq.waddr), "read and write conflict");
    }
}


/**
 * @brief First read, then write,if read write the same address, the read will get the old value
 * @return
 */
uint64_t SynReadMemorySim::update_outputs() {
    if (!read_req_seq.empty()) {
        auto read_addr = read_req_seq.back();
        read_req_seq.pop_back();
        last_read = read(aligned_addr(read_addr));
    }
    if (!write_req_seq.empty()) {
        auto write_req = this->write_req_seq.back();
        this->write_req_seq.pop_back();
        write(aligned_addr(write_req.waddr), write_req.wdata, write_req.wstrb);
    }
    return last_read;
}

bool SynReadMemorySim::load_elf(const char *file_name) {
    using namespace ELFIO;
    // Create elfio reader
    auto reader = elfio();

    // Load ELF data
    if (!reader.load(file_name)) {
        std::cout << "Can't find or process ELF file " << file_name << std::endl;
        return false;
    }

    ASSUME(reader.get_class() == ELFCLASS64);
    ASSUME(reader.get_encoding() == ELFDATA2LSB);
    ASSUME(reader.get_machine() == EM_RISCV);


    load_elf_to_mem(reader);
    collect_elf_symbols(reader);


    std::cout << "Loading elf file " << file_name << std::endl;
//    // print symbols
//    for (const auto &item: elf_symbol_map) {
//        std::cout << std::format("symbol {} value: 0x{:x}\n", item.first, item.second);
//    }

    return true;
}

void SynReadMemorySim::load_elf_to_mem(ELFIO::elfio &reader) {
    using namespace ELFIO;
    for (const auto &pseg: reader.segments) {
        if (pseg->get_type() == PT_LOAD) {
            // load segment to memory
            const char *p = pseg->get_data();
            std::memcpy(&this->mem[pseg->get_physical_address() - 0x80000000], p, pseg->get_file_size());
        }
    }
}

void SynReadMemorySim::collect_elf_symbols(ELFIO::elfio &reader) {
    using namespace ELFIO;
    for (auto &psec: reader.sections) {
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
                symbols.get_symbol(j, name, value, size, bind,
                                   type, section_index, other);
                // record symbol
                this->elf_symbol_map[name] = value;
            }
        }
    }
}


/**
 * @brief load file to memory, if the file is elf, load elf to memory, else load the file to memory
 * @param file_name
 */
void SynReadMemorySim::load_file(const char *file_name) {
    if (!load_elf(file_name)) {
        std::ifstream file(file_name, std::ios::binary);
        if (!file.is_open()) {
            std::cout << "Error: could not open file " << file_name << std::endl;
            exit(1);
        }

        std::cout << "Loading file " << file_name << std::endl;

        file.read((char *) mem.data(), mem.size());
        file.close();
    }
}

void SynReadMemorySim::dump_signature(std::string_view signature_file_name) {
    auto sig_start = elf_symbol_map.find("begin_signature");
    auto sig_end = elf_symbol_map.find("end_signature");

    if (sig_start != this->elf_symbol_map.end() && sig_end != this->elf_symbol_map.end()) {
        std::ofstream signature_file(signature_file_name.data(), std::ios::binary);
        if (!signature_file.is_open()) {
            std::cout << "Error: could not open file " << signature_file_name << std::endl;
            return;
        }


        for (auto i = sig_start->second; i < sig_end->second; i += 4) {
            uint32_t value = *reinterpret_cast<uint32_t*>(&mem[i - 0x80000000]);

            auto fmt = std::format("{:08x}\n", value);
            signature_file.write(fmt.data(), fmt.size());
        }

        signature_file.close();

        std::cout << std::format("dump signature to {},sig_start: 0x{:x}, sig_end: 0x{:x}\n",
                                 signature_file_name, sig_start->second, sig_end->second);
    }
}

void SynReadMemorySim::check_to_host(const std::function<void(void)> &exit_callback) {
    auto to_host_addr = elf_symbol_map.find("tohost");
    if (to_host_addr != this->elf_symbol_map.end()) {
        auto to_host_value = read((to_host_addr->second) - 0x80000000);
        if (to_host_value != 0) {
            std::cout << std::format("tohost value: 0x{:x}\n", to_host_value);
            exit_callback();
        }
    }
}




