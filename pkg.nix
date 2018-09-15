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
  version = "0.2.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "zephyr.cabal" ];
  libraryHaskellDepends = [
    aeson
    ansi-terminal
    base
    base-compat
    bytestring
    boxes
    containers
    directory
    filepath
    formatting
    Glob
    language-javascript
    mtl
    optparse-applicative
    purescript
    safe
    text
    transformers
    transformers-base
    transformers-compat
    utf8-string
  ];
  testHaskellDepends = [
    aeson
    ansi-terminal
    base
    base-compat
    bytestring
    containers
    directory
    filepath
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
    transformers-base
    transformers-compat
    utf8-string
    cabal-install
    QuickCheck
  ];
  license = stdenv.lib.licenses.mpl20;
  homepage = "https://github.com/coot/zephyr#readme";
  enableSeparateDocOutput = false;
}
