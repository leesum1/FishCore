{
  description = "Aggregated flake with sbt-overlay-flakes and riscv-toolchain-flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    # 引用子 Flakes https://github.com/NixOS/nix/issues/3978
    sbt-overlay-flakes.url = "path:sbt-overlay-flakes";
    riscv-toolchain-flakes.url = "path:riscv-toolchain-flakes";
    sail-riscv.url = "path:sail-riscv";
  };

  outputs = { self, nixpkgs, flake-utils, sbt-overlay-flakes, riscv-toolchain-flakes,sail-riscv }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages = {
        # 导出 sbt-overlay-flakes 的包
        sbt = sbt-overlay-flakes.packages.${system}.sbt;

        # 导出 riscv-toolchain-flakes 的包
        riscv64-none-gcc = riscv-toolchain-flakes.packages.${system}.riscv64-none-gcc;
        riscv64-none-gdb = riscv-toolchain-flakes.packages.${system}.riscv64-none-gdb;
        riscv32-none-gcc = riscv-toolchain-flakes.packages.${system}.riscv32-none-gcc;
        riscv32-none-gdb = riscv-toolchain-flakes.packages.${system}.riscv32-none-gdb;
        sail-riscv-rv32 = sail-riscv.packages.${system}.sail-riscv-rv32;
        sail-riscv-rv64 = sail-riscv.packages.${system}.sail-riscv-rv64;
      };
    });
}
