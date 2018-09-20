{ mkDerivation, aeson, aeson-better-errors, ansi-terminal
, ansi-wl-pprint, base, base-compat, blaze-html, bower-json, boxes
, bytestring, Cabal, cheapskate, clock, containers, data-ordlist
, deepseq, directory, dlist, edit-distance, fetchgit, file-embed
, filepath, fsnotify, gitrev, Glob, haskeline, hpack, hspec
, hspec-discover, http-types, HUnit, language-javascript
, lifted-base, microlens-platform, monad-control, monad-logger, mtl
, network, optparse-applicative, parallel, parsec, pattern-arrows
, process, protolude, regex-tdfa, safe, scientific, semigroups
, sourcemap, split, stdenv, stm, stringsearch, syb, tasty
, tasty-hspec, text, time, transformers, transformers-base
, transformers-compat, unordered-containers, utf8-string, vector
, wai, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "purescript";
  version = "0.12.0";
  src = fetchgit {
    url = "https://github.com/purescript/purescript";
    sha256 = "0pp53v1z9pg13lcq8l1lqf7yy9f875akp8k1dfkzcw9d84b8zcn9";
    rev = "fb8daff44cce61fb404d30695065769fef582401";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-better-errors ansi-terminal base base-compat blaze-html
    bower-json boxes bytestring Cabal cheapskate clock containers
    data-ordlist deepseq directory dlist edit-distance file-embed
    filepath fsnotify Glob haskeline language-javascript lifted-base
    microlens-platform monad-control monad-logger mtl parallel parsec
    pattern-arrows process protolude regex-tdfa safe scientific
    semigroups sourcemap split stm stringsearch syb text time
    transformers transformers-base transformers-compat
    unordered-containers utf8-string vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-better-errors ansi-terminal ansi-wl-pprint base
    base-compat blaze-html bower-json boxes bytestring Cabal cheapskate
    clock containers data-ordlist deepseq directory dlist edit-distance
    file-embed filepath fsnotify gitrev Glob haskeline http-types
    language-javascript lifted-base microlens-platform monad-control
    monad-logger mtl network optparse-applicative parallel parsec
    pattern-arrows process protolude regex-tdfa safe scientific
    semigroups sourcemap split stm stringsearch syb text time
    transformers transformers-base transformers-compat
    unordered-containers utf8-string vector wai wai-websockets warp
    websockets
  ];
  testHaskellDepends = [
    aeson aeson-better-errors ansi-terminal base base-compat blaze-html
    bower-json boxes bytestring Cabal cheapskate clock containers
    data-ordlist deepseq directory dlist edit-distance file-embed
    filepath fsnotify Glob haskeline hspec hspec-discover HUnit
    language-javascript lifted-base microlens-platform monad-control
    monad-logger mtl parallel parsec pattern-arrows process protolude
    regex-tdfa safe scientific semigroups sourcemap split stm
    stringsearch syb tasty tasty-hspec text time transformers
    transformers-base transformers-compat unordered-containers
    utf8-string vector
  ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  preConfigure = "hpack";
  homepage = "http://www.purescript.org/";
  description = "PureScript Programming Language Compiler";
  license = stdenv.lib.licenses.bsd3;
}
