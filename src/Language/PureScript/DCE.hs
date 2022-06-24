module Language.PureScript.DCE
  ( module DCE ) where

import Language.PureScript.DCE.CoreFn as DCE
    ( runDeadCodeElimination, runBindDeadCodeElimination )
import Language.PureScript.DCE.Foreign as DCE
    ( runForeignModuleDeadCodeElimination )
import Language.PureScript.DCE.Errors as DCE
    ( Level(..),
      DCEError(..),
      EntryPoint(..),
      isEntryParseError,
      showEntryPoint,
      displayDCEError,
      displayDCEWarning,
      errorColor,
      warnColor,
      codeColor,
      colorString,
      colorText )
import Language.PureScript.DCE.Eval as DCE ( evaluate )
