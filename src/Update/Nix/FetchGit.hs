{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Update.Nix.FetchGit
  ( updatesFromFile
  ) where

import           Control.Concurrent.Async    (mapConcurrently)
import           Data.Generics.Uniplate.Data
import           Data.Monoid                  ((<>))
import           Data.Text                    hiding (concat, concatMap)
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

-- | Gather the 'SpanUpdate's for a file at a particular filepath
updatesFromFile :: FilePath -> IO (Either Warning [SpanUpdate])
updatesFromFile filename = do
  t <- T.readFile filename
  case parseNixTextLoc t of
    Failure d -> pure $ Left (CouldNotParseInput d)
    Success e -> case fetchGitUpdateInfos t e of
      Left w -> pure (Left w)
      Right updateInfos ->
        sequenceA <$> mapConcurrently updateInfoToUpdate updateInfos >>= \case
          Left w -> pure $ Left w
          Right pairs -> pure $ Right (universeBi pairs)

-- | Given an expression, return all the opportunities for updates for any
-- subexpression.
fetchGitUpdateInfos :: Text
                    -> NExprLoc
                    -> Either Warning (FetchTree FetchGitUpdateInfo)
fetchGitUpdateInfos t e = do
  -- Convert the Nix AST to a tree with fetch information.
  fetchTree <- exprToFetchTree e
  -- Linearize the source file info and make the fetch data easier to use.
  -- TODO: only linearize at the very end when we are writing to the file
  traverse (fetchGitArgsToUpdate t) fetchTree

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
-- Massaging the data a little to make it easier to process
--------------------------------------------------------------------------------

-- | Given some arguments to fetchgit, extract the information necessary to
-- update it
fetchGitArgsToUpdate :: Text -> FetchGitArgs -> Either Warning FetchGitUpdateInfo
fetchGitArgsToUpdate t as = FetchGitUpdateInfo (extractUrlString (repoLocation as))
                                           <$> exprSpan t (revExpr as)
                                           <*> exprSpan t (sha256Expr as)

--------------------------------------------------------------------------------
-- Getting updated information from the internet.
--------------------------------------------------------------------------------

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
