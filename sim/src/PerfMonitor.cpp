

#include "include/PerfMonitor.h"


PerfMonitor::PerfMonitor() {
    logger = spdlog::get("console");
    logger->info("PerfMonitor init");
}

PerfMonitor::~PerfMonitor() {
    logger->info("PerfMonitor exit");
}

void PerfMonitor::add_perf_counter(CounterInfo perf_counter) {
    perf_counters.emplace_back(perf_counter);
}

void PerfMonitor::print_perf_counter() const {
    logger->info("PerfMonitor print_perf_counter");
    for (const auto& counter : perf_counters) {
        const auto hit_rate = static_cast<double>(counter.hit) / (counter.total + 1);
        logger->info("{:<10} hit_count:{:<8} total_count:{:<8} hit_rate:{:<10}",
                     counter.name.c_str(), counter.hit, counter.total, hit_rate);
    }
}
