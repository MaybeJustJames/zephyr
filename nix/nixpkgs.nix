{ compiler ? "ghc865" }:
with builtins;
let
  rev = "da1483458d632912103f884af802951fdc70bf99";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  config =
    { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ${compiler} = super.haskell.packages.${compiler}.override {
              overrides = self: super: {
                language-javascript = super.callPackage ./language-javascript.nix {};
              };
            };
          };
        };
      };
    };
  nixpkgs = import (fetchTarball { inherit url; }) { inherit config; };
in nixpkgs
