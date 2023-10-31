#include "Utils.h"


namespace Utils {
    bool check_aligned(uint64_t addr, uint64_t size) {
        return (addr & (size - 1)) == 0;
    }

    // 8 bytes aligned
    uint64_t aligned_addr(uint64_t addr) {
        return addr & ~(0x7);
    }
}