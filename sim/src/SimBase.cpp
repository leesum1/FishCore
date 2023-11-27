#include "include/SimBase.h"
#include "Vtop.h"
#include "CSREncode.h"
#include <memory>
#include <iostream>
#include "Utils.h"

SimBase::SimBase()
{
    top = std::make_shared<Vtop>();
}

void SimBase::dump_wave() const
{
#if VM_TRACE_FST == 1
    if (wave_trace_flag)
    {
        if (tfp->isOpen() && top->contextp()->time() > wave_stime * 2)
        {
            tfp->dump(top->contextp()->time());
        }
    }
#endif
    top->contextp()->timeInc(1);
}

void SimBase::enable_wave_trace(const std::string& file_name, const uint64_t wave_stime)
{
#if VM_TRACE_FST == 1
    wave_trace_flag = true;
    this->wave_stime = wave_stime;
    tfp = new VerilatedFstC;
    Verilated::traceEverOn(true);
    top->trace(tfp, 99);
    tfp->open(file_name.c_str());
#endif
}

SimBase::~SimBase()
{
#if VM_TRACE_FST == 1
    if (wave_trace_flag)
    {
        std::cout << "close wave trace file" << std::endl;
        if (tfp->isOpen())
        {
            tfp->flush();
            tfp->close();
            delete tfp;
        }
    }
#endif
}

void SimBase::reset() const
{
    top->reset = 1;

    for (int i = 0; i < 10; i++)
    {
        top->clock ^= 1;
        top->eval();
    }
    top->reset = 0;
}

uint64_t SimBase::get_pc() const
{
    return top->io_difftest_bits_pc;
}

uint64_t SimBase::get_reg(const int idx)
{
#define GET_REG(top, idx) (top->io_difftest_bits_gpr_##idx)

    switch (idx)
    {
    case 0:
        return GET_REG(top, 0);
    case 1:
        return GET_REG(top, 1);
    case 2:
        return GET_REG(top, 2);
    case 3:
        return GET_REG(top, 3);
    case 4:
        return GET_REG(top, 4);
    case 5:
        return GET_REG(top, 5);
    case 6:
        return GET_REG(top, 6);
    case 7:
        return GET_REG(top, 7);
    case 8:
        return GET_REG(top, 8);
    case 9:
        return GET_REG(top, 9);
    case 10:
        return GET_REG(top, 10);
    case 11:
        return GET_REG(top, 11);
    case 12:
        return GET_REG(top, 12);
    case 13:
        return GET_REG(top, 13);
    case 14:
        return GET_REG(top, 14);
    case 15:
        return GET_REG(top, 15);
    case 16:
        return GET_REG(top, 16);
    case 17:
        return GET_REG(top, 17);
    case 18:
        return GET_REG(top, 18);
    case 19:
        return GET_REG(top, 19);
    case 20:
        return GET_REG(top, 20);
    case 21:
        return GET_REG(top, 21);
    case 22:
        return GET_REG(top, 22);
    case 23:
        return GET_REG(top, 23);
    case 24:
        return GET_REG(top, 24);
    case 25:
        return GET_REG(top, 25);
    case 26:
        return GET_REG(top, 26);
    case 27:
        return GET_REG(top, 27);
    case 28:
        return GET_REG(top, 28);
    case 29:
        return GET_REG(top, 29);
    case 30:
        return GET_REG(top, 30);
    case 31:
        return GET_REG(top, 31);
    default:
        MY_ASSERT(false);
        return 0;
    }
}


uint64_t SimBase::get_csr(int idx)
{
    MY_ASSERT(idx < 4096, "csr index out of range");
#define GET_CSR(top, name) (top->io_difftest_bits_csr_##name)
    switch (idx)
    {
    case MISA:
        return GET_CSR(top, misa);
    case MSTATUS:
        return GET_CSR(top, mstatus);
    case MIE:
        return GET_CSR(top, mie);
    case MTVEC:
        return GET_CSR(top, mtvec);
    case MEPC:
        return GET_CSR(top, mepc);
    case MCAUSE:
        return GET_CSR(top, mcause);
    case MTVAL:
        return GET_CSR(top, mtval);
    case MEDELEG:
        return GET_CSR(top, medeleg);
    case MIDELEG:
        return GET_CSR(top, mideleg);
    case MSCRATCH:
        return GET_CSR(top, mscratch);
    case SSTATUS:
        return GET_CSR(top, mstatus);
    case SEPC:
        return GET_CSR(top, sepc);
    case SCAUSE:
        return GET_CSR(top, scause);
    case STVAL:
        return GET_CSR(top, stval);
    case STVEC:
        return GET_CSR(top, stvec);
    case SATP:
        return GET_CSR(top, satp);
    case SSCRATCH:
        return GET_CSR(top, sscratch);


    default:
        MY_ASSERT(false, "csr index out of range");
        return 0;
    }
}

void SimBase::step(
    const std::function<bool(std::shared_ptr<Vtop>)>& func
)
{
    top->clock ^= 1;
    top->eval();
    // always sample on posedge
    if (top->clock == 1 && top->reset == 0)
    {
        if (func(top))
        {
            finish_flag = true;
            top->contextp()->gotFinish(true);
        }
    }
    dump_wave();
}
