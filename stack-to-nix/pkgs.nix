{
  extras = hackage:
    {
      packages = {
        "happy" = (((hackage.happy)."1.19.9").revisions).default;
        "language-javascript" = (((hackage.language-javascript)."0.7.1.0").revisions).default;
        "purescript" = (((hackage.purescript)."0.13.8").revisions).default;
        "network" = (((hackage.network)."3.0.1.1").revisions).default;
        "these" = (((hackage.these)."1.0.1").revisions).default;
        "semialign" = (((hackage.semialign)."1").revisions).default;
        zephyr = ./zephyr.nix;
        };
      };
  resolver = "lts-13.26";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "these" = {
            flags = {
              "assoc" = lib.mkOverride 900 false;
              "quickcheck" = lib.mkOverride 900 false;
              };
            };
          "aeson-pretty" = {
            flags = { "lib-only" = lib.mkOverride 900 true; };
            };
          };
        })
    { packages = {}; }
    ];
  }