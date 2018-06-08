{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Update.Nix.FetchGit
  ( updatesFromFile
  ) where

import           Control.Concurrent.Async     (mapConcurrently)
import           Control.Error
import           Data.Foldable                (toList)
import           Data.Generics.Uniplate.Data  (para)
import           Data.Text                    (pack)
import           Nix.Expr
import           Update.Nix.FetchGit.Prefetch
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Utils
import           Update.Nix.FetchGit.Warning
import           Update.Span

--------------------------------------------------------------------------------
-- Tying it all together
--------------------------------------------------------------------------------

-- | Given the path to a Nix file, returns the SpanUpdates
-- all the parts of the file we want to update.
updatesFromFile :: FilePath -> IO (Either Warning [SpanUpdate])
updatesFromFile f = runExceptT $ do
  expr <- ExceptT $ ourParseNixFile f
  treeWithArgs <- hoistEither $ exprToFetchTree expr
  treeWithLatest <- ExceptT $
    sequenceA <$> mapConcurrently getFetchGitLatestInfo treeWithArgs
  pure (fetchTreeToSpanUpdates treeWithLatest)

--------------------------------------------------------------------------------
-- Extracting information about fetches from the AST
--------------------------------------------------------------------------------

-- Get a FetchTree from a nix expression.
exprToFetchTree :: NExprLoc -> Either Warning (FetchTree FetchGitArgs)
exprToFetchTree = para $ \e subs -> case e of
  -- If it is a call (application) of fetchgit, record the
  -- arguments since we will need to update them.
  AnnE _ (NBinary NApp function (AnnE _ (NSet bindings)))
    | extractFuncName function == Just "fetchgit"
    || extractFuncName function == Just "fetchgitPrivate"
    -> FetchNode <$> extractFetchGitArgs bindings

  -- Similarly, record calls to fetchFromGitHub.
  AnnE _ (NBinary NApp function (AnnE _ (NSet bindings)))
    | extractFuncName function == Just "fetchFromGitHub"
    -> FetchNode <$> extractFetchFromGitHubArgs bindings

  -- If it is an attribute set, find any attributes in it that we
  -- might want to update.
  AnnE _ (NSet bindings)
    -> Node <$> findAttr "version" bindings <*> sequenceA subs

  -- If this is something uninteresting, just wrap the sub-trees.
  _ -> Node Nothing <$> sequenceA subs

-- | Extract a 'FetchGitArgs' from the attrset being passed to fetchgit.
extractFetchGitArgs :: [Binding NExprLoc] -> Either Warning FetchGitArgs
extractFetchGitArgs bindings =
    FetchGitArgs <$> (URL <$> (exprText =<< extractAttr "url" bindings))
                 <*> extractAttr "rev" bindings
                 <*> extractAttr "sha256" bindings

-- | Extract a 'FetchGitArgs' from the attrset being passed to fetchFromGitHub.
extractFetchFromGitHubArgs :: [Binding NExprLoc] -> Either Warning FetchGitArgs
extractFetchFromGitHubArgs bindings =
    FetchGitArgs <$> (GitHub <$> (exprText =<< extractAttr "owner" bindings)
                             <*> (exprText =<< extractAttr "repo" bindings))
                 <*> extractAttr "rev" bindings
                 <*> extractAttr "sha256" bindings

--------------------------------------------------------------------------------
-- Getting updated information from the internet.
--------------------------------------------------------------------------------

getFetchGitLatestInfo :: FetchGitArgs -> IO (Either Warning FetchGitLatestInfo)
getFetchGitLatestInfo args = runExceptT $ do
  o <- ExceptT (nixPrefetchGit (extractUrlString $ repoLocation args))
  d <- hoistEither (parseISO8601DateToDay (date o))
  pure $ FetchGitLatestInfo args (rev o) (sha256 o) d

--------------------------------------------------------------------------------
-- Deciding which parts of the Nix file should be updated and how.
--------------------------------------------------------------------------------

fetchTreeToSpanUpdates :: FetchTree FetchGitLatestInfo -> [SpanUpdate]
fetchTreeToSpanUpdates node@(Node _ cs) =
  concatMap fetchTreeToSpanUpdates cs ++
  toList (maybeUpdateVersion node)
fetchTreeToSpanUpdates (FetchNode f) = [revUpdate, sha256Update]
  where revUpdate = SpanUpdate (exprSpan (revExpr args))
                               (quoteString (latestRev f))
        sha256Update = SpanUpdate (exprSpan (sha256Expr args))
                                  (quoteString (latestSha256 f))
        args = originalInfo f

-- Given a node of the fetch tree which might contain a version
-- string, decides whether and how that version string should be
-- updated.  We basically just take the maximum latest commit date of
-- all the fetches in the children.
maybeUpdateVersion :: FetchTree FetchGitLatestInfo -> Maybe SpanUpdate
maybeUpdateVersion node@(Node (Just versionExpr) _) = do
  maxDay <- (maximumMay . fmap latestDate . toList) node
  pure $ SpanUpdate (exprSpan versionExpr) ((quoteString . pack . show) maxDay)
maybeUpdateVersion _ = Nothing
