{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Update.Nix.FetchGit
  ( updatesFromFile
  , processFile
  ) where

import           Control.Concurrent.Async       ( mapConcurrently )
import           Control.Error
import           Data.Foldable                  ( toList )
import           Data.Generics.Uniplate.Data    ( para )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Nix.Expr
import           Nix.Match.Typed
import           Update.Nix.FetchGit.Prefetch
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Utils
import           Update.Nix.FetchGit.Warning
import           Update.Span

import qualified Data.Text.IO
import qualified System.Exit
import qualified System.IO
import Data.Fix


--------------------------------------------------------------------------------
-- Tying it all together
--------------------------------------------------------------------------------

-- | Provided FilePath, update Nix file in-place
processFile :: FilePath -> [Text] -> IO ()
processFile filename args = do
  t <- Data.Text.IO.readFile filename
  -- Get the updates from this file.
  updatesFromFile filename args >>= \case
    -- If we have any errors, print them and finish.
    Left  ws -> printErrorAndExit ws
    Right us ->
      -- Update the text of the file in memory.
                case updateSpans us t of
        -- If updates are needed, write to the file.
      t' | t' /= t -> do
        Data.Text.IO.writeFile filename t'
        putStrLn $ "Made " ++ (show $ length us) ++ " changes"

      _ -> putStrLn "No updates"
 where
  printErrorAndExit :: Warning -> IO ()
  printErrorAndExit e = do
    System.IO.hPutStrLn System.IO.stderr (formatWarning e)
    System.Exit.exitFailure

-- | Given the path to a Nix file, returns the SpanUpdates
-- all the parts of the file we want to update.
updatesFromFile :: FilePath -> [Text] -> IO (Either Warning [SpanUpdate])
updatesFromFile f extraArgs = runExceptT $ do
  expr           <- ExceptT $ ourParseNixFile f
  treeWithArgs   <- hoistEither $ exprToFetchTree expr
  treeWithLatest <-
    ExceptT
    $   sequenceA
    <$> mapConcurrently (getFetchGitLatestInfo extraArgs) treeWithArgs
  pure (fetchTreeToSpanUpdates treeWithLatest)

--------------------------------------------------------------------------------
-- Extracting information about fetches from the AST
--------------------------------------------------------------------------------

exprToFetchTree :: NExprLoc -> Either Warning (FetchTree FetchGitArgs)
exprToFetchTree = \case
  [matchNixLoc|
    ^fetcher {
      url = ^url;
      rev = ^rev;
      sha256 = ^sha256;
    }|] | extractFuncName fetcher `elem` [Just "fetchgit", Just "fetchgitPrivate"]
    -> do
      url' <- URL <$> exprText url
      pure $ FetchNode (FetchGitArgs url' rev (Just sha256))
  [matchNixLoc|
    ^fetcher {
      url = ^url;
      rev = ^rev;
    }|] | extractFuncName fetcher `elem` [Just "fetchGit"]
    -> do
      url' <- URL <$> exprText url
      pure $ FetchNode (FetchGitArgs url' rev Nothing)
  [matchNixLoc|
    ^fetcher {
      owner = ^owner;
      repo = ^repo;
      rev = ^rev;
      sha256 = ^sha256;
    }|] | Just funName <- extractFuncName fetcher
        , Just fun <- case funName of
                        "fetchFromGitHub" -> Just GitHub
                        "fetchFromGitLab" -> Just GitLab
                        _ -> Nothing
    -> do
      owner' <- exprText owner
      repo' <- exprText repo
      pure $ FetchNode (FetchGitArgs (fun owner' repo') rev (Just sha256))
  e@[matchNixLoc|{
      _version = ^version;
    }|] -> Node version <$> traverse exprToFetchTree (toList (unFix e))
  e -> Node Nothing <$> traverse exprToFetchTree (toList (unFix e))

--------------------------------------------------------------------------------
-- Getting updated information from the internet.
--------------------------------------------------------------------------------

getFetchGitLatestInfo
  :: [Text] -> FetchGitArgs -> IO (Either Warning FetchGitLatestInfo)
getFetchGitLatestInfo extraArgs args = runExceptT $ do
  o <- ExceptT (nixPrefetchGit extraArgs (extractUrlString $ repoLocation args))
  d <- hoistEither (parseISO8601DateToDay (date o))
  pure $ FetchGitLatestInfo args (rev o) (sha256 o) d

--------------------------------------------------------------------------------
-- Deciding which parts of the Nix file should be updated and how.
--------------------------------------------------------------------------------

fetchTreeToSpanUpdates :: FetchTree FetchGitLatestInfo -> [SpanUpdate]
fetchTreeToSpanUpdates node@(Node _ cs) =
  concatMap fetchTreeToSpanUpdates cs ++ toList (maybeUpdateVersion node)
fetchTreeToSpanUpdates (FetchNode f) = catMaybes [Just revUpdate, sha256Update]
 where
  revUpdate = SpanUpdate (exprSpan (revExpr args)) (quoteString (latestRev f))
  sha256Update = SpanUpdate <$> (exprSpan <$> sha256Expr args) <*> Just
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
