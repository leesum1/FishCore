#pragma once

#include "spdlog/spdlog.h"


class PerfMonitor {
    struct CounterInfo {
        std::string name;
        uint64_t hit;
        uint64_t total;
    };


    std::shared_ptr<spdlog::logger> logger;
    std::vector<CounterInfo> perf_counters = {};

public:
    PerfMonitor();
    ~PerfMonitor();

    // pair.first: hit
    // pair.second: total
    void add_perf_counter(CounterInfo perf_counter);
    void print_perf_counter() const;
};



