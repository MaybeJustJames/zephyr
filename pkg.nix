{ mkDerivation
, nixpkgs
, stdenv

, bower
, cabal-install
, git
, npm

, aeson
, ansi-terminal
, base
, base-compat
, bytestring
, boxes
, containers
, directory
, filepath
, formatting
, Glob
, hspec
, hspec-core
, HUnit
, language-javascript
, mtl
, optparse-applicative
, process
, purescript
, QuickCheck
, safe
, text
, transformers
, transformers-base
, transformers-compat
, utf8-string
}:
mkDerivation {
  pname = "zephyr";
  version = "0.2.2";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "zephyr.cabal" ];
  libraryHaskellDepends = [
    aeson
    ansi-terminal
    base
    base-compat
    boxes
    containers
    formatting
    language-javascript
    mtl
    purescript
    safe
    text
  ];
  testHaskellDepends = [
    aeson
    base
    base-compat
    containers
    directory
    hspec
    hspec-core
    HUnit
    language-javascript
    mtl
    optparse-applicative
    process
    purescript
    text
    transformers
    utf8-string
    QuickCheck
  ];
  license = stdenv.lib.licenses.mpl20;
  homepage = "https://github.com/coot/zephyr#readme";
  enableSeparateDocOutput = false;
}
