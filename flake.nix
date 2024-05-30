# in flake.nix# in flake.nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }@inputs:
    let
      overlay = import ./overlay.nix;
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system ;
            overlays = [ overlay ];
            config.allowUnfree = true;
          };
          pythonPackages = pkgs.python3Packages;
          circt1_62_0 = (import (builtins.fetchTarball {
              url = "https://github.com/NixOS/nixpkgs/archive/7995cae3ad60e3d6931283d650d7f43d31aaa5c7.tar.gz";
              sha256 = "0811ph8hddkk9jg8ing6z56v50k351p5i3f50g0x1600ja5fyzdm";
          }) {inherit system ;}).circt;
        in
        with pkgs;
        {
          devShells.default = mkShell {
            name = "python-venv";
            venvDir = "./.venv";
            buildInputs = [
              # A Python interpreter including the 'venv' module is required to bootstrap
              # the environment.
              pythonPackages.python
              # This executes some shell code to initialize a venv in $venvDir before
              # dropping into the shell
              pythonPackages.venvShellHook
              verilator
              espresso
              gnumake
              xmake
              jdk11
              # circt
              # circt1_62_0
              (sbt.override {jre = jdk11;})
              git
              which
            ];
            # Run this command, only after creating the virtual environment
            postVenvCreation = ''
              unset SOURCE_DATE_EPOCH
              # pip install -r requirements.txt
            '';

            # Now we can execute any commands within the virtual environment.
            # This is optional and can be left out to run pip manually.
            postShellHook = ''
              # allow pip to install wheels
              unset SOURCE_DATE_EPOCH
              pip install -r requirements.txt
            '';
          };
        }
      )
      // { inherit inputs; overlays.default = overlay; };
}


