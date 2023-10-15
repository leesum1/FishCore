//
// Created by leesum on 10/14/23.
//




#include "SynReadMemorySim.hpp"
#include <catch2/catch_all.hpp>


TEST_CASE("Test load_elf with valid ELF file", "[load_elf]") {
// Replace 'your_valid_elf_file' with the path to a valid ELF file for testing
    std::string_view file_name = "/home/leesum/workhome/ysyx/am-kernels/tests/cpu-tests/build/recursion-riscv64-nemu.elf";
    SynReadMemorySim sim(0x1000); // Create an instance of your class

// Perform the test
    REQUIRE_NOTHROW(sim.load_file(file_name.data())); // Verify that no exceptions are thrown

//// Additional assertions based on your requirements
//REQUIRE(/* Add your custom assertions here */);
}