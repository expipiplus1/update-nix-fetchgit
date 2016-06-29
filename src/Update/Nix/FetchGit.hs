{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Update.Nix.FetchGit
  ( updatesFromFile
  ) where

import           Data.Generics.Uniplate.Data
import           Data.Monoid                  ((<>))
import           Data.Text                    hiding (concat, concatMap)
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

--------------------------------------------------------------------------------
-- The data type we pass around and update
--------------------------------------------------------------------------------

-- | A place to update with any sub-sources. The sub-sources are used in
-- getting the latest commit date for updating a version tag.
--
-- The type parameter is the type used to represent fetchgit calls
data UpdateSource git = VersionSet{ versionExpr :: NExprLoc
                                  , sources     :: [UpdateSource git]
                                  }
                      | FetchGit{ args    :: git
                                , sources :: [UpdateSource git]
                                }
  deriving (Show, Data, Functor, Foldable, Traversable)

--------------------------------------------------------------------------------
-- Tying it all together
--------------------------------------------------------------------------------

-- | Gather the 'SpanUpdate's for a file at a particular filepath
updatesFromFile :: FilePath -> IO (Either Warning [SpanUpdate])
updatesFromFile filename = do
  t <- T.readFile filename
  case parseNixTextLoc t of
    Failure d -> pure $ Left (CouldNotParseInput d)
    Success e -> case fetchGitUpdateInfos t e of
      Left w -> pure (Left w)
      Right updateInfos ->
        sequenceA2 <$> mapConcurrently2 updateInfoToUpdate updateInfos >>= \case
          Left w -> pure $ Left w
          Right pairs -> pure $ Right (universeBi pairs)

-- | Given an expression, return all the opportunities for updates for any
-- subexpression.
fetchGitUpdateInfos :: Text
                    -> NExprLoc
                    -> Either Warning [UpdateSource FetchGitUpdateInfo]
fetchGitUpdateInfos t e = do
  -- Get the 'FetchGitArgs' values from the tree
  argTrees <- exprToUpdateSource e
  -- Get the 'FetchGitUpdateInfo' values
  traverse2 (fetchGitArgsToUpdate t) argTrees

--------------------------------------------------------------------------------
-- Extracting places to update from the AST
--------------------------------------------------------------------------------

-- | A fetchgit, fetchgitPrivate or fetchFromGitHub value's expressions
data FetchGitArgs = FetchGitArgs{ repoLocation :: RepoLocation
                                , revExpr      :: NExprLoc
                                , sha256Expr   :: NExprLoc
                                }
  deriving (Show, Data)

-- Get a set of 'UpdateSource' trees from a nix expression.
exprToUpdateSource :: NExprLoc -> Either Warning [UpdateSource FetchGitArgs]
exprToUpdateSource = sequenceA . para exprChildrenToUpdateSources

-- Given an expression and the update sources contained in its children, return
-- a new set of update sources
exprChildrenToUpdateSources :: NExprLoc
                            -> [[Either Warning (UpdateSource FetchGitArgs)]]
                            -> [Either Warning (UpdateSource FetchGitArgs)]
exprChildrenToUpdateSources e subss =
  let subs = concat subss
  in case e of

      -- If this is a call to some symbol
      AnnE _ (NApp (AnnE _ (NSym fg)) a)
        -- And the symbol is a name for fetchgit
        | fg `elem` fetchgitCalleeNames
        -> [FetchGit <$> extractFetchGitArgs a <*> sequenceA subs]

      -- If this is an attr set with a version attribute, remember where
      -- the version attribute was and set the sub sources
      AnnE _ (NSet bindings)
        | Right versionAttr <- extractAttr "version" bindings
        -> [VersionSet versionAttr <$> sequenceA subs]

      -- If this is something else, just forward the sub sources
      _somethingUninteresting -> subs

-- | The names for fetchgit like values.
fetchgitCalleeNames :: [Text]
fetchgitCalleeNames = ["fetchgit", "fetchgitPrivate"]

-- | Extract a 'FetchGitArgs' from the attrset being passed to fetchgit.
extractFetchGitArgs :: NExprLoc -> Either Warning FetchGitArgs
extractFetchGitArgs = \case
  AnnE _ (NSet bindings) ->
    FetchGitArgs <$> (URL <$> (exprText =<< extractAttr "url" bindings))
                 <*> extractAttr "rev" bindings
                 <*> extractAttr "sha256" bindings
  e -> Left (ArgNotASet e)

--------------------------------------------------------------------------------
-- Massaging the data a little to make it easier to process
--------------------------------------------------------------------------------

-- | The info needed to find the latest git version and the info about where to
-- update the rev and sha256.
data FetchGitUpdateInfo = FetchGitUpdateInfo{ urlString :: Text
                                            , revPos    :: SourceSpan
                                            , sha256Pos :: SourceSpan
                                            }
  deriving (Show)

-- | Given some arguments to fetchgit, extract the information necessary to
-- update it
fetchGitArgsToUpdate :: Text -> FetchGitArgs -> Either Warning FetchGitUpdateInfo
fetchGitArgsToUpdate t as = FetchGitUpdateInfo (extractUrlString (repoLocation as))
                                           <$> exprSpan t (revExpr as)
                                           <*> exprSpan t (sha256Expr as)

--------------------------------------------------------------------------------
-- The type we have after calling nix-prefetch-git, A pair of SpanUpdates for
-- each fetchgit call.
--------------------------------------------------------------------------------

-- | A pair of 'SpanUpdate's for updating a single fetchgit value.
data FetchGitSpanUpdates = FetchGitSpanUpdates{ revUpdate    :: SpanUpdate
                                              , sha256Update :: SpanUpdate
                                              }
  deriving (Show, Data)

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


