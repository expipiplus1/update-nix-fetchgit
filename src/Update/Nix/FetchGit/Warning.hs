module Update.Nix.FetchGit.Warning
  ( Warning(..)
  ) where

import           Data.Text
import           Nix.Expr

data Warning = CouldNotParseInput Text
             | MissingAttr Text
             | DuplicateAttrs Text
             | NotAString NExprLoc
             | NotABool NExprLoc
             | NixPrefetchGitFailed Int Text
             | InvalidPrefetchGitOutput Text
             | NixPrefetchUrlFailed Int Text
             | InvalidPrefetchUrlOutput Text
             | InvalidDateString Text
             | GitLsRemoteFailed Int Text
             | NoSuchRef Text
             | InvalidGitLsRemoteOutput Text
  deriving Show
