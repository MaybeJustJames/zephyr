{ compiler ? "ghc843" }:
with builtins;
let
  rev = "61deecdc34fc609d0f805b434101f3c8ae3b807a";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  # nix-prefetch-url --unpack
  sha256 = "147xyn8brvkfgz1z3jbk13w00h6camnf6z0bz0r21g9pn1vv7sb0";
  config =
    { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ${compiler} = super.haskell.packages.${compiler}.override {
              overrides = self: super: {
                spdx = lib.dontCheck (super.callPackage ./spdx.nix {});
                # purescript fb8daf 
                purescript = super.callPackage ./purescript.nix {};
              };
            };
          };
        };
      };
    };
  nixpkgs = import (fetchTarball { inherit url sha256; }) { inherit config; };
in nixpkgs
