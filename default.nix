# To regenerate ./stack-to-nix/
# 1. nix-shell
# 2. stack-to-nix --output stack-to-nix
# 3. rm -f stack-to-nix/default.nix

let
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};

  # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;

  # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ pkgs ? import nixpkgsSrc nixpkgsArgs
}:

let

pkgSet = pkgs.haskell-nix.mkStackPkgSet {
  stack-pkgs = import ./stack-to-nix/pkgs.nix;
  pkg-def-extras = [
    (hackage: { hsc2hs = hackage.hsc2hs."0.68.4".revisions.default; }) # fixes https://github.com/input-output-hk/haskell.nix/issues/214#issuecomment-515785254
  ];
  modules = [];
};

in
pkgSet.config.hsPkgs.zephyr
