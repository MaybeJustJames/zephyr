module Main (main) where

import Prelude ()
import Prelude.Compat

import qualified TestDCE

import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  TestDCE.main
