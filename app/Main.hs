module Main (main) where

import           Data.Version (showVersion)
import qualified Options.Applicative as Opts
import qualified Paths_zephyr as Paths
import           System.Environment (getArgs)
import qualified System.IO as IO

import Command.Run ( runZephyr )
import Command.Options ( parseOptions )


main :: IO ()
main = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8
  let pinfo = Opts.info (versionOpt <*> Opts.helper <*> parseOptions)
                        (Opts.progDesc "tree-shaking breeze for PureScript")
  getArgs
    >>= Opts.handleParseResult . execParserPure pinfo
    >>= runZephyr
  where
    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo [] = Opts.Failure $ 
      Opts.parserFailure Opts.defaultPrefs pinfo (Opts.ShowHelpText Nothing) mempty
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

versionOpt :: Opts.Parser (a -> a)
versionOpt = Opts.abortOption (Opts.InfoMsg versionString) $
     Opts.long "version"
  <> Opts.help "Show the version number"
  <> Opts.hidden
  where
  versionString = showVersion Paths.version
