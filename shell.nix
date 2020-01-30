{ compiler ? "ghc865"
, haddock ? true
, test ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix {};
  default = import ./default.nix {inherit compiler haddock test;};
in
  {
    zephyr = if nixpkgs.lib.inNixShell
      then default.zephyr.env
      else default.zephyr;
  }
