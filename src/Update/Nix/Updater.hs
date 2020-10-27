{-# LANGUAGE QuasiQuotes #-}
-- | This module exports a list of 'Fetcher's, programs which match on nix
-- fetching expressions and return programs to update them
module Update.Nix.Updater
  ( fetchers
  ) where

import           Data.Maybe
import           Data.Text                      ( Text
                                                , splitOn
                                                )
import           Nix                            ( NExprLoc )
import           Nix.Comments
import           Nix.Match.Typed
import qualified Update.Nix.FetchGit.Prefetch  as P
import           Update.Nix.FetchGit.Prefetch   ( Revision(..)
                                                , getGitFullName
                                                , getGitHubRevisionDate
                                                , getGitRevision
                                                , nixPrefetchGit
                                                , nixPrefetchUrl
                                                )
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Utils
import           Update.Span

type Fetcher = (NExprLoc -> Maybe Comment) -> NExprLoc -> Maybe (M Updater)

fetchers :: (NExprLoc -> Maybe Comment) -> [NExprLoc -> Maybe (M Updater)]
fetchers getComment =
  ($ getComment)
    <$> [ fetchgitUpdater
        , builtinsFetchGitUpdater
        , fetchTarballGithubUpdater
        , builtinsFetchTarballUpdater
        , fetchGitHubUpdater
        ]

fetchgitUpdater :: Fetcher
fetchgitUpdater getComment = \case
  [matchNixLoc|
    ^fetcher {
      url = ^url;
      rev = ^rev; # rev
      sha256 = ^sha256;
    }|] | extractFuncName fetcher `elem` [Just "fetchgit", Just "fetchgitPrivate"]
    -> Just $ do
      url' <- fromEither $ URL <$> exprText url
      let desiredRev = commentToRequest (getComment rev)
      pure $ gitUpdater url' desiredRev rev (Just sha256)
  _ -> Nothing

builtinsFetchGitUpdater :: Fetcher
builtinsFetchGitUpdater getComment = \case
  [matchNixLoc|
    ^fetcher {
      url = ^url;
      rev = ^rev; # rev
    }|] | Just "fetchGit" <- extractFuncName fetcher
    -> Just $ do
      url' <- fromEither $ URL <$> exprText url
      let desiredRev = commentToRequest (getComment rev)
      pure $ gitUpdater url' desiredRev rev Nothing
  _ -> Nothing

fetchTarballGithubUpdater :: Fetcher
fetchTarballGithubUpdater getComment = \case
  [matchNixLoc|
    ^fetcher {
      url = ^url; # rev
      sha256 = ^sha256;
    }|]
    | Just "fetchTarball" <- extractFuncName fetcher
    , Right url' <- exprText url
    , "https:" : "" : "github.com" : owner : repo : "archive" : _ <- splitOn
      "/"
      url'
    , comment <- getComment url
    , comment /= Just "pin" -- Fall back to the regular tarball updater if we've been instructed to not change this URL
    -> Just $ do
      let rev = Revision $ fromMaybe "HEAD" comment
          repoUrl = "https://github.com/" <> owner <> "/" <> repo
      pure . Updater $ do
        revision <- getGitRevision repoUrl rev
        let newUrl = repoUrl <> "/archive/" <> revision <> ".tar.gz"
        let Updater u = tarballUpdater newUrl sha256
        date <- getGitHubRevisionDate owner repo (Revision revision)
        (_, urlUpdate) <- u
        pure (Just date, SpanUpdate (exprSpan url) (quoteString newUrl) : urlUpdate)
  _ -> Nothing

builtinsFetchTarballUpdater :: Fetcher
builtinsFetchTarballUpdater _ = \case
  [matchNixLoc|
    ^fetcher {
      url = ^url;
      sha256 = ^sha256;
    }|] | Just "fetchTarball" <- extractFuncName fetcher
    -> Just $ do
      url' <- fromEither $ exprText url
      pure $ tarballUpdater url' sha256
  _ -> Nothing

fetchGitHubUpdater :: Fetcher
fetchGitHubUpdater getComment = \case
  [matchNixLoc|
    ^fetcher {
      owner = ^owner;
      repo = ^repo;
      rev = ^rev;
      sha256 = ^sha256;
    }|] | Just fun <- extractFuncName fetcher >>= \case
                        "fetchFromGitHub" -> Just GitHub
                        "fetchFromGitLab" -> Just GitLab
                        _ -> Nothing
    -> Just $ do
      owner' <- fromEither $ exprText owner
      repo' <- fromEither $ exprText repo
      let desiredRev = commentToRequest (getComment rev)
      pure $ gitUpdater (fun owner' repo') desiredRev rev (Just sha256)
  _ -> Nothing

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data RevisionRequest
  = Pin
  | DoNotPin Revision

commentToRequest :: Maybe Text -> Maybe RevisionRequest
commentToRequest = \case
  Nothing    -> Nothing
  Just "pin" -> Just Pin
  Just r     -> Just (DoNotPin (Revision r))

gitUpdater
  :: RepoLocation
  -- ^ Repo URL
  -> Maybe RevisionRequest
  -- ^ Desired revision
  -> NExprLoc
  -- ^ rev
  -> Maybe NExprLoc
  -- ^ sha256, not present for some fetchers
  -> Updater
gitUpdater repoLocation revisionRequest revExpr sha256Expr = Updater $ do
  let repoUrl = extractUrlString repoLocation
  logVerbose $ "Updating " <> prettyRepoLocation repoLocation
  revArgs <- case revisionRequest of
    Nothing  -> pure []
    Just req -> do
      rev <- case req of
        Pin        -> fromEither (exprText revExpr)
        DoNotPin r -> getGitFullName repoUrl r
      pure ["--rev", rev]
  o <- nixPrefetchGit revArgs repoUrl
  d <- fromEither $ parseISO8601DateToDay (P.date o)
  pure
    ( Just d
    , [ SpanUpdate (exprSpan e) (quoteString (P.sha256 o))
      | Just e <- pure sha256Expr
      ]
      <> [SpanUpdate (exprSpan revExpr) (quoteString $ P.rev o)]
    )

tarballUpdater
  :: Text
  -- ^ URL
  -> NExprLoc
  -- ^ sha256
  -> Updater
tarballUpdater url sha256Expr = Updater $ do
  logVerbose $ "Updating " <> url
  sha256 <- nixPrefetchUrl [] url
  pure (Nothing, [SpanUpdate (exprSpan sha256Expr) (quoteString sha256)])
