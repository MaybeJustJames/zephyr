{-# LANGUAGE CPP #-}

module Main (main) where

import System.IO (hSetEncoding, stderr, stdout, utf8)
import qualified Test.CoreFn
import qualified Test.Eval
import Test.Hspec
import qualified Test.Lib
#ifdef TEST_CORE_LIBS
import qualified Test.CoreLib
#endif

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  hspec Test.CoreFn.spec
  hspec Test.Eval.spec
  hspec Test.Lib.spec

-- Tree shaking of core libraries is disabled by default because it's not
-- reliable.
#ifdef TEST_CORE_LIBS
  hspec Test.CoreLib.spec
#endif
