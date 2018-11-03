{}:
with builtins;
let
  rev = "a715aa7073c683cb791763b88f7e9af43a276963";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  config =
    { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
          overrides = self: super: {
            spdx = lib.dontCheck (super.callPackage ./spdx.nix {});
          };
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc844 = super.haskell.packages.ghc844.override { inherit overrides; };
            ghc861 = super.haskell.packages.ghc861.override { inherit overrides; };
          };
        };
      };
    };
  nixpkgs = import (fetchTarball { inherit url; }) { inherit config; };
in nixpkgs
