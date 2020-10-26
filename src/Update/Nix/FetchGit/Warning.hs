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
             | NixPrefetchUrlFailed Int Text
             | InvalidPrefetchUrlOutput Text
             | InvalidDateString Text
             | GitLsRemoteFailed Int Text
             | NoSuchRef Text
             | InvalidGitLsRemoteOutput Text
  deriving Show
