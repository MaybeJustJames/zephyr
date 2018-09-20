{ mkDerivation, base, containers, fetchgit, QuickCheck, stdenv
, tasty, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "spdx";
  version = "0.2.2.0";
  src = fetchgit {
    url = "https://github.com/phadej/spdx";
    sha256 = "1nssnix61lpc7m6xi089ds4awqg28d1qfz8irvz60yw40giajy9r";
    rev = "4288df6e4b7840eb94d825dcd446b42fef25ef56";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base containers transformers ];
  testHaskellDepends = [ base tasty tasty-quickcheck ];
  benchmarkHaskellDepends = [ base QuickCheck tasty-quickcheck ];
  homepage = "https://github.com/phadej/spdx";
  description = "SPDX license expression language";
  license = stdenv.lib.licenses.bsd3;
}
