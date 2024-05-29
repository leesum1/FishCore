#pragma once

#include <cstdint>
#include <cassert>
#include <cstdio>

namespace Utils {


#define MY_ASSERT(expr, ...)  \
    if (!(expr)) {             \
        printf(__VA_ARGS__);   \
        assert(0);             \
    }

    bool check_aligned(uint64_t addr, uint64_t size);

    uint64_t aligned_addr(uint64_t addr);
}

