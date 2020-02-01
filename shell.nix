{ compiler ? "ghc865"
, haddock ? true
, test ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix {};
  lib = nixpkgs.haskell.lib;
  default = import ./default.nix {inherit compiler haddock test;};
in { zephyr = lib.shellAware default.zephyr; }
