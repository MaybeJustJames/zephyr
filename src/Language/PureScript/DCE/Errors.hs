module Language.PureScript.DCE.Errors
  ( DCEError(..)
  , displayDCEError
  , displayDCEWarning
  , Level(..)
  )
  where

import Prelude.Compat

import           Data.Char (isSpace)
import           Data.List (dropWhileEnd)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.PureScript.AST.SourcePos (SourceSpan(..), displaySourceSpan)
import           Language.PureScript.Names
import           Language.PureScript.PSString
import           Language.PureScript.CoreFn.Ann
import qualified Text.PrettyPrint.Boxes as Box

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

formatDCEError :: DCEError a -> Text
formatDCEError (IdentifierNotFound _ _ qi) = "Unknown value " <> showQualified runIdent qi <> "."
formatDCEError (ArrayIdxOutOfBound _ _ i) = "Array literal lacks required index " <> T.pack (show i) <> "."
formatDCEError (AccessorNotFound _ _ acc) = "Object literal lacks required label " <> prettyPrintString acc <> "."
formatDCEError NoEntryPointFound = "No entry point found."
formatDCEError (EntryPointsNotFound qis) = "Entry points " <> T.intercalate ", " (showQualified runIdent `map` qis) <> " not found."

renderDCEError :: FilePath -> DCEError a -> Box.Box
renderDCEError relPath err =
  case getAnn err of
    Just (mn, ss) -> paras
      [ line $ "in module " <> runModuleName mn
      , line $ "at " <> displaySourceSpan relPath ss
      , indent $ line $ formatDCEError err
      ]
    Nothing -> paras [ line $ formatDCEError err ]

displayDCEError :: FilePath -> DCEError 'Error -> String
displayDCEError relPath err = renderBox $ paras
  [ line "Error"
  , indent $ renderDCEError relPath err
  ]

displayDCEWarning :: FilePath -> DCEError 'Warning -> String
displayDCEWarning relPath err = renderBox $ paras
  [ line $ "Warning"
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
