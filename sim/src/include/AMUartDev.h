#pragma once


#include "DeviceBase.h"

namespace SimDevices
{
    class AMUartDev final : public DeviceBase
    {
        uint64_t mem_addr;
        uint64_t mem_size;

    public:
        explicit AMUartDev(uint64_t base);

        void update_inputs(
            uint64_t read_addr,
            bool read_en,
            WriteReq write_req,
            bool write_en
        ) override;

        uint64_t update_outputs() override;

        bool in_range(uint64_t addr) override;

        std::vector<AddrInfo> get_addr_info() override;
    };
}


