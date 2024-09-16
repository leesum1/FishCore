#include "AllTask.h"
#include <cstdint>
#include <cstdlib>

static std::shared_ptr<spdlog::logger> console = nullptr;

// // see spike fesvr/device.h class command_t
// // see https://github.com/riscv-software-src/riscv-isa-sim/issues/364
// // Bits 63:56 indicate the "device".
// // Bits 55:48 indicate the "command".
// // Device 0 is the syscall device, which is used to emulate Unixy syscalls.
// //  It only implements command 0, which has two subfunctions (for legacy
// reasons, sorry for the bad design):
// //  If bit 0 is clear, then bits 47:0 represent a pointer to a struct
// describing the syscall. The format of the syscall struct is pretty clear if
// you read the pk or fesvr code.
// //  If bit 0 is set, then bits 47:1 represent an exit code, with a zero value
// indicating success and other values indicating failure.
// // Device 1 is the blocking character device.
// //  Command 0 reads a character
// //  Command 1 writes a character from the 8 LSBs of tohost

struct FesvrCmd {
  uint8_t cmd;
  uint8_t device;
  uint16_t tohost;
  uint32_t unknown;

  uint64_t value;

  FesvrCmd(uint64_t value) { update(value); }
  void update(uint64_t value) {

    tohost = (value & 0x000000000000ffffull);
    unknown = ((value >> 16) & 0x00000000ffffffffull);
    cmd = ((value >> 48) & 0x00000000000000ffull);
    device = ((value >> 56) & 0x00000000000000ffull);

    this->value = value;
  }

  bool is_character_device() const { return device == 1; }
  bool is_syscall_device() const { return device == 0; }

  uint64_t exit_code() const { return (value & 0x0000ffffffffffffull) >> 1; }

  void character_device_write() const {
    if (cmd == 1 && is_character_device()) {
      std::cout << static_cast<char>(tohost);
    }
  }
  std::optional<bool> syscall_device() const {
    if (cmd == 0 && is_syscall_device() && (tohost & 1) == 1) {
      return exit_code() == 0;
    } else {
      return std::nullopt;
    }
  }
};

void task_tohost_check(SimBase &sim_base, SimDevices::SynReadMemoryDev &sim_mem,
                       bool to_host_check_enabled) {

  static auto tohost_callback_func = [&sim_mem,
                                      &sim_base](uint64_t tohost_data) {
    sim_mem.check_to_host([&](const uint64_t to_host_data) {
      // sim_base.set_state(SimBase::sim_stop);

      static FesvrCmd fesvr_cmd(to_host_data);
      fesvr_cmd.update(to_host_data);

      console->debug("Write tohost {:x} at pc {:x}", to_host_data,
                     sim_base.get_pc());

      console->debug("device: {}, cmd: {}, tohost: {}, unknown: {}",
                     fesvr_cmd.device, fesvr_cmd.cmd, fesvr_cmd.tohost,
                     fesvr_cmd.unknown);

      if (auto pass = fesvr_cmd.syscall_device(); pass.has_value()) {
        if (pass.value()) {
          sim_base.set_state(SimBase::sim_stop);
          console->info("PASS");
        } else {
          sim_base.set_state(SimBase::sim_abort);
          console->info("FAIL WITH EXIT CODE:{}", fesvr_cmd.exit_code());
        }
      }
      fesvr_cmd.character_device_write();
    });
  };

  if (to_host_check_enabled) {
    console = spdlog::get("console");

    sim_base.add_once_time_task(
        {.task_func =
             [&sim_base, &sim_mem] {
               auto top = sim_base.top;
               if (sim_mem.get_to_host_addr().has_value()) {
                 top->io_tohost_addr_bits = sim_mem.get_to_host_addr().value();
                 top->io_tohost_addr_valid = true;
                 console->info("Set to_host_addr: 0x{:016x}\n",
                               sim_mem.get_to_host_addr().value());
               } else {
                 top->io_tohost_addr_valid = false;
                 top->io_tohost_addr_bits = 0;
                 console->info("to_host_addr not found\n");
                 exit(-1);
               }
             },
         .name = "Set to_host_addr",
         .period_cycle = 0,
         .type = SimTaskType::once});

    // for riscof and riscv-tests, use to_host to communicate with simulation
    // environment
    sim_base.add_after_clk_rise_task({.task_func =
                                          [&sim_mem, &sim_base] {
                                            sim_mem.check_to_host(
                                                tohost_callback_func);
                                          },
                                      .name = "to_host_check",
                                      .period_cycle = 1024,
                                      .type = SimTaskType::period});
  }
}