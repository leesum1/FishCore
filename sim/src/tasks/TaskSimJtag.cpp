#include "AllTask.h"

void task_simjtag(bool rbb_en, int rbb_port, SimBase &sim_base,
                  std::optional<RemoteBitBang> &rbb_simjtag) {
  if (rbb_en) {
    rbb_simjtag.emplace(rbb_port);

    sim_base.add_after_clk_rise_task(
        {.task_func =
             [&] {
               const auto top = sim_base.top;

               unsigned char *jtag_tck_ptr =
                   static_cast<unsigned char *>(&(top->io_jtag_io_tck));
               unsigned char *jtag_tms_ptr =
                   static_cast<unsigned char *>(&(top->io_jtag_io_tms));
               unsigned char *jtag_tdi_ptr =
                   static_cast<unsigned char *>(&(top->io_jtag_io_tdi));
               unsigned char *jtag_tdo_ptr =
                   static_cast<unsigned char *>(&(top->io_jtag_io_tdo));

               top->io_jtag_io_tdi_en = 1;
               static unsigned char last_tclk = *jtag_tck_ptr;
               rbb_simjtag->tick(jtag_tck_ptr, jtag_tms_ptr, jtag_tdi_ptr,
                                 *jtag_tdo_ptr);
             },
         .name = "simjtag",
         .period_cycle = 20,
         .type = SimTaskType::period});
  }
}