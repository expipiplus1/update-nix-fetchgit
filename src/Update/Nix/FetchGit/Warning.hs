module Update.Nix.FetchGit.Warning
  ( Warning(..)
  ) where

import           Data.Text
import           Nix.Expr
import           Text.PrettyPrint.ANSI.Leijen (Doc)

data Warning = CouldNotParseInput Doc
             | ArgNotASet NExprLoc
             | MissingAttr Text
             | DuplicateAttrs Text
             | NotAString NExprLoc
             | BadSourcePos Delta
             | NixPrefetchGitFailed Int Text
             | InvalidPrefetchGitOutput Text
             | InvalidDateString Text
  deriving (Show)
