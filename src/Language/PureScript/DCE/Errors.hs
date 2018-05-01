module Language.PureScript.DCE.Errors
  ( DCEError(..)
  , displayDCEError
  , displayDCEWarning
  , Level(..)
  , warnColor
  , errorColor
  , codeColor
  , colorString
  , colorText
  )
  where

import Prelude.Compat

import           Data.Char (isSpace)
import           Data.List (intersperse, dropWhileEnd)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting (sformat, string, stext, (%))
import           Language.PureScript.AST.SourcePos
                  ( SourceSpan(..)
                  , displaySourceSpan
                  )
import           Language.PureScript.Names
import           Language.PureScript.PSString
import           Language.PureScript.CoreFn.Ann
import qualified Text.PrettyPrint.Boxes as Box
import qualified System.Console.ANSI as ANSI

data Level = Error | Warning deriving (Show)

data DCEError (a :: Level)
  = IdentifierNotFound ModuleName Ann (Qualified Ident)
  | ArrayIdxOutOfBound ModuleName Ann Integer
  | AccessorNotFound ModuleName Ann PSString
  | NoEntryPointFound
  | EntryPointsNotFound [Qualified Ident]
  deriving (Show)

getAnn :: DCEError a -> Maybe (ModuleName, SourceSpan)
getAnn (IdentifierNotFound mn (ss, _, _, _) _) = Just (mn, ss)
getAnn (ArrayIdxOutOfBound mn (ss, _, _, _) _) = Just (mn, ss)
getAnn (AccessorNotFound mn (ss, _, _, _) _) = Just (mn, ss)
getAnn _ = Nothing

formatDCEError :: DCEError a -> Box.Box
formatDCEError (IdentifierNotFound _ _ qi) =
          Box.text "Unknown value "
  Box.<+> colorBox codeColor (T.unpack $ showQualified runIdent qi)
  Box.<> "."
formatDCEError (ArrayIdxOutOfBound _ _ i) =
          Box.text "Array literal lacks required index"
  Box.<+> colorBox codeColor (show i)
  Box.<> "."
formatDCEError (AccessorNotFound _ _ acc) =
          Box.text "Object literal lacks required label"
  Box.<+> colorBox codeColor (T.unpack $ prettyPrintString acc)
  Box.<> "."
formatDCEError NoEntryPointFound = "No entry point found."
formatDCEError (EntryPointsNotFound qis) =
          Box.text ("Entry point" ++ if length qis > 1 then "s:" else "")
  Box.<+> foldr1 (Box.<>) (intersperse (Box.text ", ")
            $ map (colorBox codeColor . T.unpack . showQualified runIdent) qis)
  Box.<+> "not found."

renderDCEError :: FilePath -> DCEError a -> Box.Box
renderDCEError relPath err =
  case getAnn err of
    Just (mn, ss) -> paras
      [ Box.text "in module"
          Box.<+> colorBox codeColor (T.unpack (runModuleName mn))
      , line $ "at " <> displaySourceSpan relPath ss
      , indent $ formatDCEError err
      ]
    Nothing -> paras [ formatDCEError err ]

displayDCEError :: FilePath -> DCEError 'Error -> String
displayDCEError relPath err = renderBox $ paras
  [ colorBox errorColor "Error"
  , indent $ renderDCEError relPath err
  ]

displayDCEWarning :: FilePath -> (Int, Int) ->  DCEError 'Warning -> String
displayDCEWarning relPath (idx, count) err = renderBox $ paras
  [ colorBox warnColor "Warning"
      Box.<+> Box.text (show idx ++ " of " ++ show count)
  , indent $ renderDCEError relPath err
  ]

paras :: [Box.Box] -> Box.Box
paras = Box.vcat Box.left

line :: Text -> Box.Box
line = Box.text . T.unpack

renderBox :: Box.Box -> String
renderBox = unlines
            . map (dropWhileEnd isSpace)
            . dropWhile whiteSpace
            . dropWhileEnd whiteSpace
            . lines
            . Box.render
  where
  whiteSpace = all isSpace

indent :: Box.Box -> Box.Box
indent = Box.moveUp 1 . Box.moveDown 1 . Box.moveRight 2

ansiColor :: (ANSI.ColorIntensity, ANSI.Color, ANSI.ConsoleIntensity) -> String
ansiColor (colorIntensity, color, consoleIntensity) = ANSI.setSGRCode
  [ ANSI.SetColor ANSI.Foreground colorIntensity color
  , ANSI.SetConsoleIntensity consoleIntensity
  ]

ansiColorReset :: String
ansiColorReset = ANSI.setSGRCode [ANSI.Reset]

errorColor :: (ANSI.ColorIntensity, ANSI.Color, ANSI.ConsoleIntensity)
errorColor = (ANSI.Dull, ANSI.Red, ANSI.BoldIntensity)

warnColor :: (ANSI.ColorIntensity, ANSI.Color, ANSI.ConsoleIntensity)
warnColor = (ANSI.Dull, ANSI.Yellow, ANSI.BoldIntensity)

codeColor :: (ANSI.ColorIntensity, ANSI.Color, ANSI.ConsoleIntensity)
codeColor = (ANSI.Dull, ANSI.Yellow, ANSI.NormalIntensity)

colorString
  :: (ANSI.ColorIntensity, ANSI.Color, ANSI.ConsoleIntensity)
  -> String
  -> String
colorString color s = ansiColor color ++ s ++ ansiColorReset

colorText
  :: (ANSI.ColorIntensity, ANSI.Color, ANSI.ConsoleIntensity)
  -> Text
  -> Text
colorText color t = sformat
  (string%stext%string) (ansiColor color) t ansiColorReset

colorBox
  :: (ANSI.ColorIntensity, ANSI.Color, ANSI.ConsoleIntensity)
  -> String
  -> Box.Box
colorBox color = Box.text . colorString color
