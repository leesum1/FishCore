#pragma once

#include <assert.hpp>

namespace Utils {


#define MY_ASSERT(expr, ...)  ASSERT(expr, __VA_ARGS__)

    bool check_aligned(uint64_t addr, uint64_t size) {
        return (addr & (size - 1)) == 0;
    }

}

