{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Update.Nix.FetchGit.Types where

import           Data.Data (Data)
import           Data.Text (Text)
import qualified Data.Time (Day)
import           Nix.Expr  (NExprLoc)

-- | The day portion of a date, with no timezone information.
type Day = Data.Time.Day

-- | A tree with a structure similar to the AST of the Nix file we are
-- parsing, but which only contains the information we care about.
-- The fetchInfo type parameter allows this tree to be used at
-- different stages in the program where we know different amounts of
-- information about a fetch expression.
data FetchTree fetchInfo = Node { nodeVersionExpr :: Maybe NExprLoc
                                , nodeChildren    :: [FetchTree fetchInfo]
                                }
                         | FetchNode fetchInfo
  deriving (Show, Data, Functor, Foldable, Traversable)

-- | Represents the arguments to a call to a fetcher as parsed from a .nix
-- file. sha256Expr will be empty on calls to builtins.fetchGit.
data FetchArgs
  = FetchGitArgs
    { repoLocation :: RepoLocation
    , revExpr      :: NExprLoc
    , sha256Expr   :: Maybe NExprLoc
    , ref          :: Maybe Text
    }
  | FetchTarballArgs
    { tarballLocation :: Text
    , sha256Expr      :: Maybe NExprLoc
    }
  deriving (Show, Data)

-- | Updated information about a fetcher call that was retrieved from the
-- internet.
data FetchLatestInfo
  = FetchGitLatestInfo
    { originalInfo :: FetchArgs
    , latestRev    :: Text
    , latestSha256 :: Text
    , latestDate   :: Day
    }
  | FetchTarballLatestInfo
    { originalInfo :: FetchArgs
    , latestSha256 :: Text
    }
  deriving (Show, Data)

-- | A repo is either specified by URL or by Github owner/repo.
data RepoLocation = URL Text
                  | GitHub { owner :: Text
                           , repo  :: Text
                           }
                  | GitLab { owner :: Text
                           , repo  :: Text
                           }
  deriving (Show, Data)
