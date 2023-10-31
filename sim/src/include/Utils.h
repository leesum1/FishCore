#pragma once

#include <assert.hpp>
#include <cstdint>

namespace Utils {

#define MY_ASSERT(expr, ...)  ASSUME(expr, __VA_ARGS__)

    bool check_aligned(uint64_t addr, uint64_t size);

    uint64_t aligned_addr(uint64_t addr);
}

