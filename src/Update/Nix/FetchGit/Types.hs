{-# OPTIONS_GHC -fno-warn-orphans #-}

module Update.Nix.FetchGit.Types where

import           Control.Monad.Reader
import           Control.Monad.Validate
import           Control.Monad.Validate.Internal
import           Data.Bifunctor                 ( Bifunctor(first) )
import           Data.Functor
import           Data.Monoid
import           Data.Text                      ( Text )
import           Data.Time                      ( Day )
import           Nix.Expr                       ( NExprLoc )
import           Update.Nix.FetchGit.Warning
import           Update.Span

type M = ReaderT Env (ValidateT (Dual [Warning]) IO)

runM :: Env -> M a -> IO ([Warning], Maybe a)
runM env = fmap (first (reverse . getDual)) . asWarnings . flip runReaderT env

-- | Runs a 'ValidateT' computation returning the errors raised by 'refute' or
-- 'dispute' if any, as well as returning the computation’s result if possible.
asWarnings :: (Functor m, Monoid e) => ValidateT e m a -> m (e, Maybe a)
asWarnings m = unValidateT MNothing m <&> \case
  Left  e             -> (e, Nothing)
  Right (MJust e , a) -> (e, Just a)
  Right (MNothing, a) -> (mempty, Just a)

data Env = Env
  { sayLog :: Verbosity -> Text -> IO ()
  , updateLocations :: [(Int, Int)]
  }

data Verbosity
  = Verbose
  | Normal
  | Quiet

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

