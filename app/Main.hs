module Main where

import           Command.DCE
import           Options.Applicative ((<**>))
import qualified Options.Applicative as Opts
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8
  let info = Opts.info (dceOptions <**> Opts.helper) (Opts.progDesc "tree shaking breeze for PureScript")
  opts <- Opts.execParser info
  runDCECommand opts
