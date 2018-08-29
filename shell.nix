{ compiler ? "ghc822"
, haddock ? true
, test ? true
, benchmarks ? false
}:
with builtins;
let
  default = import ./default.nix {inherit compiler haddock test;};
  spec = fromJSON (readFile ./nixpkgs.json);
  src = fetchTarball {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    sha256 = spec.sha256;
  };
  nixpkgs = import src {};
in
  {
    zephyr = if nixpkgs.lib.inNixShell
      then default.zephyr.env
      else default.zephyr;
  }
