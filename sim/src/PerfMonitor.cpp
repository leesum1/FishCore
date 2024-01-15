

#include "include/PerfMonitor.h"


PerfMonitor::PerfMonitor() {
    logger = spdlog::get("console");
    perf_trace = spdlog::get("perf_trace");
    logger->info("PerfMonitor init Finished");
}

PerfMonitor::~PerfMonitor() {
    logger->info("PerfMonitor exit");
}

void PerfMonitor::add_perf_counter(CounterInfo perf_counter) {
    perf_counters.emplace_back(perf_counter);
}

void PerfMonitor::print_perf_counter(const bool use_log) const {
    const auto log_select = use_log ? logger : perf_trace;

    for (const auto& counter : perf_counters) {
        const auto hit = *counter.hit;
        const auto total = *counter.total;

        const auto hit_rate = static_cast<double>(hit) / total;
        log_select->info("{:<10} hit_count:{:<8} total_count:{:<8} hit_rate:{:<10}",
                         counter.name.c_str(), hit, total, hit_rate);
    }
    log_select->info("");
}

