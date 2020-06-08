{ compiler ? "ghc865"
, haddock ? true
, test ? true
, benchmarks ? false
, pkgs ? import ./nix/pkgs.nix { inherit compiler; }
}:
with builtins;
let
  hsLib = pkgs.haskell.hsLib;
  default = import ./default.nix { inherit compiler haddock test pkgs; };
in { zephyr = hsLib.shellAware default.zephyr; }
