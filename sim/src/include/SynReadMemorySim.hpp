#include <cstdint>
#include <vector>
#include <fstream>
#include <iostream>
#include "Utils.hpp"

class SynReadMemorySim
{
private:
    const uint32_t mem_size;
    std::vector<uint8_t> mem;


    std::vector<uint64_t> read_req;
    struct WriteReq {
        uint64_t waddr;
        uint64_t wdata;
        uint8_t wstrb;
    };
    std::vector<WriteReq> write_req;


    uint64_t last_read = 0;

    uint64_t aligned_addr(uint64_t addr) {
        return addr & ~(0x7);
    }

public:
    SynReadMemorySim(uint32_t mem_size);

    void update_inputs(
        uint64_t read_addr,
        bool read_en,
        WriteReq write_req,
        bool write_en
    );

    uint64_t update_outputs();

    void load_file(std::string file_name) {
        std::ifstream file(file_name, std::ios::binary);
        if (!file.is_open()) {
            std::cerr << "Error: could not open file " << file_name << std::endl;
            exit(1);
        }
        file.read((char*)mem.data(), mem.size());
        file.close();
    }




    uint64_t read(uint64_t addr);
    void write(uint64_t addr, uint64_t wdata, uint8_t wstrb);

    ~SynReadMemorySim();
};

SynReadMemorySim::SynReadMemorySim(uint32_t mem_size) : mem_size(mem_size) {
    mem = std::vector<uint8_t>(mem_size);
    MY_ASSERT(mem.size() == mem_size);
}

SynReadMemorySim::~SynReadMemorySim() {


}

uint64_t SynReadMemorySim::read(uint64_t addr) {
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
        read_req.emplace_back(read_addr);
    }
    if (write_en) {
        this->write_req.emplace_back(write_req);
    }

    if (read_en && write_en) {
//        MY_ASSERT(aligned_addr(read_addr) != aligned_addr(write_req.waddr), "read and write conflict");
    }
}


/**
 * @brief First read, then write,if read write the same address, the read will get the old value
 * @return
 */
uint64_t SynReadMemorySim::update_outputs() {
    if (!read_req.empty()) {
        auto read_addr = read_req.back();
        read_req.pop_back();
        last_read = read(aligned_addr(read_addr));
    }
    if (!write_req.empty()) {
        auto write_req = this->write_req.back();
        this->write_req.pop_back();
        write(aligned_addr(write_req.waddr), write_req.wdata, write_req.wstrb);
    }
    return last_read;
}