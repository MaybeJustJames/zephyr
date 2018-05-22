{nixpkgs ? import <nixpkgs> {}, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "zephyr";
  buildInputs = [ zlib git nodejs nodePackages.npm nodePackages.bower ];
  inherit ghc;
}
