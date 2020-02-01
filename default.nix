{ compiler ? "ghc865"
, haddock ? false
, test ? false
, benchmarks ? false
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix { inherit compiler; };

  pkgs = nixpkgs.haskell.packages;
  lib = nixpkgs.haskell.lib;

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else nixpkgs.lib.id;

  zephyr = lib.enableCabalFlag (doHaddock(doTest(doBench(
    pkgs.${compiler}.callPackage ./pkg.nix {
      inherit nixpkgs;
    })))) "test-with-cabal";

in { inherit zephyr; }
