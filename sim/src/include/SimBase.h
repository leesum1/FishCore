#pragma once

#include "Vtop.h"
#include "verilated.h"
#include <string>

#if VM_TRACE_FST == 1

#include "verilated_fst_c.h"

#endif

#include <functional>
#include <Utils.h>
#include <format>

class SimBase {
    std::shared_ptr<Vtop> top;
    bool finish_flag = false;


#if VM_TRACE_FST == 1
    VerilatedFstC *tfp;
    bool wave_trace_flag = false;
#endif

public:
    SimBase();

    void enable_wave_trace(std::string file_name);

    void dump_wave();

    void step(std::function<bool(std::shared_ptr<Vtop>)> func);

    void reset();

    uint64_t get_pc();

    uint64_t get_reg(int idx);

    uint64_t get_csr(int idx);


    bool finished() {
        return finish_flag;
    }

    ~SimBase();

};

