{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Update.Nix.FetchGit
  ( updatesFromFile
  ) where

import           Control.Concurrent.Async     (mapConcurrently)
import           Data.Generics.Uniplate.Data
import qualified Data.Text.IO                 as T
import           Nix.Expr
import           Nix.Parser                   (Result (..), parseNixTextLoc)
import           Update.Nix.FetchGit.Prefetch
import           Update.Nix.FetchGit.Utils
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Warning
import           Update.Span

--------------------------------------------------------------------------------
-- Tying it all together
--------------------------------------------------------------------------------

-- | Gather the 'SpanUpdate's for a file at a particular filepath.
updatesFromFile :: FilePath -> IO (Either Warning [SpanUpdate])
updatesFromFile filename = do
  t <- T.readFile filename
  case parseNixTextLoc t of
    Failure parseError -> pure $ Left (CouldNotParseInput parseError)
    Success expr -> case exprToFetchTree expr of
      Left scanError -> pure (Left scanError)
      Right treeWithArgs ->
        sequenceA <$> mapConcurrently getFetchGitLatestInfo treeWithArgs >>= \case
          Left getLatestInfoError -> pure $ Left getLatestInfoError
          Right treeWithLatest -> pure $ return (fetchTreeToSpanUpdates treeWithLatest)

--------------------------------------------------------------------------------
-- Extracting information about fetches from the AST
--------------------------------------------------------------------------------

-- Get a FetchTree from a nix expression.
exprToFetchTree :: NExprLoc -> Either Warning (FetchTree FetchGitArgs)
exprToFetchTree = para exprToFetchTreeCore

exprToFetchTreeCore :: NExprLoc
                    -> [Either Warning (FetchTree FetchGitArgs)]
                    -> Either Warning (FetchTree FetchGitArgs)
exprToFetchTreeCore e subs =
  case e of
      -- If it is a call (application) of fetchgit, record the
      -- arguments since we will need to update them.
      AnnE _ (NApp (AnnE _ (NSym fg)) a)
        | fg `elem` ["fetchgit", "fetchgitPrivate"]
        -> FetchNode <$> extractFetchGitArgs a

      -- If it is an attribute set, find any attributes in it that we
      -- might want to update.
      AnnE _ (NSet bindings)
        -> Node <$> findAttr "version" bindings <*> sequenceA subs

      -- If this is something uninteresting, just wrap the sub-trees.
      _ -> Node Nothing <$> sequenceA subs

-- | Extract a 'FetchGitArgs' from the attrset being passed to fetchgit.
extractFetchGitArgs :: NExprLoc -> Either Warning FetchGitArgs
extractFetchGitArgs = \case
  AnnE _ (NSet bindings) ->
    FetchGitArgs <$> (URL <$> (exprText =<< extractAttr "url" bindings))
                 <*> extractAttr "rev" bindings
                 <*> extractAttr "sha256" bindings
  e -> Left (ArgNotASet e)

--------------------------------------------------------------------------------
-- Getting updated information from the internet.
--------------------------------------------------------------------------------

getFetchGitLatestInfo :: FetchGitArgs -> IO (Either Warning FetchGitLatestInfo)
getFetchGitLatestInfo args = do
  result <- nixPrefetchGit (extractUrlString $ repoLocation args)
  case result of
    Left error -> pure $ Left error
    Right o -> pure $ return $ FetchGitLatestInfo args (rev o) (sha256 o)

--------------------------------------------------------------------------------
-- Deciding which parts of the Nix file should be updated and how.
--------------------------------------------------------------------------------

fetchTreeToSpanUpdates :: FetchTree FetchGitLatestInfo -> [SpanUpdate]
fetchTreeToSpanUpdates (Node _ cs) = concatMap fetchTreeToSpanUpdates cs
fetchTreeToSpanUpdates (FetchNode f) = [revUpdate, sha256Update]
  where revUpdate = SpanUpdate (exprSpan (revExpr args))
                               (quoteString (latestRev f))
        sha256Update = SpanUpdate (exprSpan (sha256Expr args))
                                  (quoteString (latestSha256 f))
        args = (originalInfo f)
