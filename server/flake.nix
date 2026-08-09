{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";

    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  };

  outputs =
    inputs@{ naersk, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      top@{
        config,
        withSystem,
        moduleWithSystem,
        ...
      }:
      {
        imports = [ inputs.devshell.flakeModule ];

        systems = [ "x86_64-linux" ];

        perSystem =
          {
            config,
            pkgs,
            ...
          }:
          {
            # For `nix build` & `nix run`:
            packages = {
              secretdemoclub = (pkgs.callPackage naersk { }).buildPackage {
                src = ./.;
              };
              default = config.packages.secretdemoclub;
            };

            # For `nix develop` (optional, can be skipped):
            devshells.default = {
              packages = with pkgs; [
                rustc
                cargo
              ];
            };
          };
      }
    );
}
