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
             | NixShellFailed Int
             | InvalidPrefetchGitOutput Text
  deriving (Show)
