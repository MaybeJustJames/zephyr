{ compiler ? "ghc865"
, haddock ? false
, test ? false
, benchmarks ? false
, pkgs ? import ./nix/pkgs.nix { inherit compiler; }
}:
with builtins;
let
  hsPkgs = pkgs.haskell.packages;
  lib = pkgs.haskell.lib;

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else pkgs.lib.id;

  zephyr = lib.enableCabalFlag (doHaddock(doTest(doBench(
    hsPkgs.${compiler}.callPackage ./pkg.nix {
      inherit pkgs; purescript = lib.traceValShow(hsPkgs.purescript);
    })))) "test-with-cabal";

in { inherit zephyr; }
