#pragma once

#include "Vtop.h"
#include <string>

#if VM_TRACE_FST == 1

#include "verilated_fst_c.h"

#endif

#include <functional>
#include <format>

class SimBase {
    bool finish_flag = false;


#if VM_TRACE_FST == 1
    VerilatedFstC* tfp = nullptr;
    bool wave_trace_flag = false;
    uint64_t wave_stime = 0;
#endif

public:
    std::shared_ptr<Vtop> top;

    SimBase();

    void enable_wave_trace(const std::string& file_name, const uint64_t wave_stime);

    void dump_wave() const;

    void step(const std::function<bool(std::shared_ptr<Vtop>)>& func);

    void reset() const;

    uint64_t get_pc() const;

    uint64_t get_reg(int idx);

    uint64_t get_csr(int idx);


    bool finished() const {
        return finish_flag;
    }

    ~SimBase();
};
