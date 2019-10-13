module Update.Nix.FetchGit.Warning
  ( Warning(..)
  ) where

import           Data.Text
import           Nix.Expr
import           Data.Text.Prettyprint.Doc
import           Data.Void

data Warning = CouldNotParseInput (Doc Void)
             | MissingAttr Text
             | DuplicateAttrs Text
             | NotAString NExprLoc
             | NixPrefetchGitFailed Int Text
             | InvalidPrefetchGitOutput Text
             | InvalidDateString Text
