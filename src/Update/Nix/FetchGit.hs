{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Update.Nix.FetchGit
  ( updatesFromFile
  ) where

import           Control.Concurrent.Async     (mapConcurrently)
import           Data.Generics.Uniplate.Data
import           Data.Monoid                  ((<>))
import           Data.Text
import qualified Data.Text.IO                 as T
import           Nix.Expr
import           Nix.Parser                   (Result (..), parseNixTextLoc)
import           Update.Nix.FetchGit.Prefetch
import           Update.Nix.FetchGit.Utils
import           Update.Nix.FetchGit.Warning
import           Update.Span

-- TODO: Remove when using base 4.9, necessary at the moment because Compose
-- doesn't have a data declaration
import           Data.Data
import           Data.Functor.Compose
deriving instance (Typeable f, Typeable g, Typeable a, Data (f (g a)))
  => Data (Compose f g a)

-- | Information describing a repo location on Github
data GithubPath = GithubPath{ owner :: Text
                            , repo  :: Text
                            }
                            deriving (Show)

-- | A fetchgit, fetchgitPrivate or fetchFromGitHub value's expressions
data FetchGitArgs = FetchGitArgs{ urlExpr    :: Either NExprLoc GithubPath
                                , revExpr    :: NExprLoc
                                , sha256Expr :: NExprLoc
                                }
  deriving (Show)

-- | The info needed to find the latest git version and the info about where to
-- update the rev and sha256.
data FetchGitUpdateInfo = FetchGitUpdateInfo{ urlString :: Text
                                            , revPos    :: SourceSpan
                                            , sha256Pos :: SourceSpan
                                            }
  deriving (Show)

-- | A pair of 'SpanUpdate's for updating a single fetchgit value.
data FetchGitSpanUpdates = FetchGitSpanUpdates{ revUpdate    :: SpanUpdate
                                              , sha256Update :: SpanUpdate
                                              }
  deriving (Show)

-- | Gather the 'SpanUpdate's for a file at a particular filepath
updatesFromFile :: FilePath -> IO (Either Warning [SpanUpdate])
updatesFromFile filename = do
  t <- T.readFile filename
  case parseNixTextLoc t of
    Failure d -> pure $ Left (CouldNotParseInput d)
    Success e -> do
      let Right updateInfos = fetchGitUpdateInfos t e
      sequenceA <$> mapConcurrently updateInfoToUpdate updateInfos >>= \case
        Left w -> pure $ Left w
        Right pairs -> pure $ Right [u | FetchGitSpanUpdates{..} <- pairs
                                    , u <- [revUpdate, sha256Update]
                                    ]

-- | Create a pair of 'SpanUpdate's given a 'FetchGitUpdateInfo'
updateInfoToUpdate :: FetchGitUpdateInfo -> IO (Either Warning FetchGitSpanUpdates)
updateInfoToUpdate ui = do
  o <- nixPrefetchGit (urlString ui)
  pure $ prefetchGitOutputToSpanUpdate ui <$> o

-- | Use the output of nix-prefetch-git to create a pair of updates
prefetchGitOutputToSpanUpdate :: FetchGitUpdateInfo
                              -> NixPrefetchGitOutput
                              -> FetchGitSpanUpdates
prefetchGitOutputToSpanUpdate u o =
  FetchGitSpanUpdates (SpanUpdate (revPos u) (quote $ rev o))
                      (SpanUpdate (sha256Pos u) (quote $ sha256 o))
  where quote t = "\"" <> t <> "\""

-- | Given an expression, return all the opportunities for updates for any
-- subexpression.
fetchGitUpdateInfos :: Text -> NExprLoc -> Either Warning [FetchGitUpdateInfo]
fetchGitUpdateInfos t e = do
  ass <- fetchGitValues e
  traverse (fetchGitArgsToUpdate t) ass

-- | Given some arguments to fetchgit, extract the information necessary to
-- update it
fetchGitArgsToUpdate :: Text -> FetchGitArgs -> Either Warning FetchGitUpdateInfo
fetchGitArgsToUpdate t as = FetchGitUpdateInfo <$> extractUrlString (urlExpr as)
                                               <*> exprSpan t (revExpr as)
                                               <*> exprSpan t (sha256Expr as)

-- | Get the url from either a nix expression for the url or a repo and owner
-- expression.
extractUrlString :: Either NExprLoc GithubPath -> Either Warning Text
extractUrlString = \case
  Left e -> exprText e
  Right (GithubPath o r) -> error "Todo: github urls"

-- | The names for fetchgit like values.
fetchgitCalleeNames :: [Text]
fetchgitCalleeNames = ["fetchgit", "fetchgitPrivate"]

-- | Extract the calls to `fetchgit` in a nix expression
fetchGitValues :: NExprLoc -> Either Warning [FetchGitArgs]
fetchGitValues e =
  let exps = universe e
      fetchGitArgs = [a | AnnE _ (NApp (AnnE _ (NSym fg)) a) <- exps
                        , fg `elem` fetchgitCalleeNames
                        ]
  in traverse extractFetchGitArgs fetchGitArgs

-- | Extract a 'FetchGitArgs' from the attrset being passed to fetchgit.
extractFetchGitArgs :: NExprLoc -> Either Warning FetchGitArgs
extractFetchGitArgs = \case
  AnnE _ (NSet bindings) ->
    FetchGitArgs <$> (Left <$> extractAttr "url" bindings)
                 <*> extractAttr "rev" bindings
                 <*> extractAttr "sha256" bindings
  e -> Left (ArgNotASet e)

