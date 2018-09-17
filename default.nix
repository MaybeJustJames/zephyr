{ compiler ? "ghc822"
, haddock ? false
, test ? false
, benchmarks ? false
}:
with builtins;
let
  spec = fromJSON (readFile ./nixpkgs.json);
  src = fetchTarball {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    sha256 = spec.sha256;
  };
  nixpkgs = import src {};

  pkgs = nixpkgs.haskell.packages;
  lib = nixpkgs.haskell.lib;

  QuickCheck = pkgs.${compiler}.callPackage ./QuickCheck-2.12.1.nix { };

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
      inherit nixpkgs QuickCheck;
      bower = nixpkgs.nodePackages.bower;
      npm   = nixpkgs.nodePackages.npm;
    })))) "test-with-cabal";

in { inherit zephyr; }
