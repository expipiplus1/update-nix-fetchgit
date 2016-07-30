module Update.Nix.FetchGit.Warning
  ( Warning(..)
  ) where

import           Data.Text
import           Nix.Expr
import           Text.PrettyPrint.ANSI.Leijen (Doc)

data Warning = CouldNotParseInput Doc
             | MissingAttr Text
             | DuplicateAttrs Text
             | NotAString NExprLoc
             | NixPrefetchGitFailed Int Text
             | InvalidPrefetchGitOutput Text
             | InvalidDateString Text
