{
  description = "sbt with jdk17(graalvm17)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs-graalvm17.url = "https://github.com/nixos/nixpkgs/archive/4ab8a3de296914f3b631121e9ce3884f1d34e1e5.tar.gz";
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-graalvm17,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        graalvm17 = nixpkgs-graalvm17.legacyPackages.${system}.graalvmCEPackages.graalvm17-ce-full;
      in
      {
        packages = {
          # Flakes can export multiple packages. To include specific packages in
          # devbox.json you can use url fragments (e.g. path:my-flake#my-package)
          sbt = (pkgs.sbt.override { jre = graalvm17; });
          # If you only want to export a single package, you can name it default which allows
          # installation without using url fragment (.e.g. "path:my-flake")
          # default = pkgs.php.withExtensions ({ enabled, all }: enabled ++ (with all; [ ds memcached ]));
        };
      }
    );
}
