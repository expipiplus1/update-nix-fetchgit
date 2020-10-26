{-# OPTIONS_GHC -fno-warn-orphans #-}

module Update.Nix.FetchGit.Types where

import           Control.Monad.Validate
import           Data.Bifunctor                 ( Bifunctor(first) )
import           Data.Monoid
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           Nix.Expr                       ( NExprLoc )
import           Update.Nix.FetchGit.Warning
import           Update.Span

type M = ValidateT (Dual [Warning]) IO

runM :: M a -> IO (Either [Warning] a)
runM = fmap (first (reverse . getDual)) . runValidateT

newtype Updater = Updater
  { unUpdater :: M (Maybe Day, [SpanUpdate])
  }

-- | A tree with a structure similar to the AST of the Nix file we are
-- parsing, but which only contains the information we care about.
data FetchTree
  = Node { nodeVersionExpr :: Maybe NExprLoc
         , nodeChildren    :: [FetchTree]
         }
  | UpdaterNode Updater

-- | A repo is either specified by URL or by Github owner/repo.
data RepoLocation = URL Text
                  | GitHub { repoOwner :: Text
                           , repoRepo  :: Text
                           }
                  | GitLab { repoOwner :: Text
                           , repoRepo  :: Text
                           }
  deriving Show

