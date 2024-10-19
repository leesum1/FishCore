{
  description = "A flake that outputs PHP with memcached and ds extension and hello pkg.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        sail-riscv-rv32 = pkgs.sail-riscv-rv32.overrideAttrs (oldAttrs: {

          version = "master";
          src = pkgs.fetchFromGitHub {
            owner = "riscv";
            repo = oldAttrs.pname;
            rev = "c61351e16e803d5ae8c14781d13a4bf214eb51d7";
          };
          patches = [];
        });

        sail-riscv-rv64 = pkgs.sail-riscv-rv64.overrideAttrs (oldAttrs: {
          src = pkgs.fetchFromGitHub {
            owner = "riscv";
            repo = oldAttrs.pname;
            rev = "c61351e16e803d5ae8c14781d13a4bf214eb51d7";
            sha256 = "sha256-7PZNNUMaCZEBf0lOCqkquewRgZPooBOjIbGF7JlLnEo=";
          };
          patches = [];
        });


      in
      {
        packages = {
          sail-riscv-rv32 = sail-riscv-rv32;
          sail-riscv-rv64 = sail-riscv-rv64;
          # If you only want to export a single package, you can name it default which allows
          # installation without using url fragment (.e.g. "path:my-flake")
          # default = pkgs.php.withExtensions ({ enabled, all }: enabled ++ (with all; [ ds memcached ]));
        };
      }
    );
}
