{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Update.Nix.FetchGit
  ( updatesFromFile
  ) where

import           Control.Concurrent.Async     (mapConcurrently)
import           Data.Foldable                (toList)
import           Data.Generics.Uniplate.Data
import           Data.Text                    (pack)
import           Nix.Expr
import           Update.Nix.FetchGit.Prefetch
import           Update.Nix.FetchGit.Utils
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Warning
import           Update.Span
import           Control.Error

--------------------------------------------------------------------------------
-- Tying it all together
--------------------------------------------------------------------------------

-- | Given the contents of a Nix file, returns the SpanUpdates
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
maybeUpdateVersion node@(Node (Just versionExpr) _) =
  case (fmap latestDate . universeBi) node of
    [] -> Nothing
    days -> Just $ SpanUpdate (exprSpan versionExpr)
                              ((quoteString . pack . show . maximum) days)
maybeUpdateVersion _ = Nothing
