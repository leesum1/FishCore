#pragma once

#include "Vtop.h"
#include <string>
#if VM_TRACE_FST == 1

#include "verilated_fst_c.h"

#endif

#include <functional>
#include <format>

class SimBase {
public:
    struct SimTask_t {
        std::function<void()> task_func;
        std::string name;
        uint64_t period_cycle;
        uint64_t counter = 0;
    };

    enum SimState_t {
        sim_run,
        sim_stop,
        sim_abort,
        sim_finish
    };

private:
#if VM_TRACE_FST == 1
    VerilatedFstC* tfp = nullptr;
    bool wave_trace_flag = false;
    uint64_t wave_stime = 0;
#endif
    std::vector<SimTask_t> after_clk_rise_tasks;
    std::vector<SimTask_t> before_clk_rise_tasks;
    SimState_t sim_state = sim_stop;

public:
    std::shared_ptr<Vtop> top;
    uint64_t commit_num = 0;
    uint64_t not_commit_num = 0;
    uint64_t cycle_num = 0;

    SimBase();

    void enable_wave_trace(const std::string& file_name, uint64_t wave_stime);

    void dump_wave() const;

    void step();

    void reset();

    void set_state(SimState_t state);

    [[nodiscard]] SimState_t get_state() const;

    uint64_t get_pc() const;

    uint64_t get_reg(int idx);

    uint64_t get_csr(int idx);


    bool finished() const;

    void add_after_clk_rise_task(const SimTask_t& task);
    void add_before_clk_rise_task(const SimTask_t& task);

    ~SimBase();
};
