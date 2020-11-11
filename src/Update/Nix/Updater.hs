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

type Fetcher
  = Bool -> (NExprLoc -> Maybe Comment) -> NExprLoc -> Maybe (M Updater)

fetchers
  :: Bool -> (NExprLoc -> Maybe Comment) -> [NExprLoc -> Maybe (M Updater)]
fetchers onlyCommented getComment =
  ($ getComment)
    .   ($ onlyCommented)
    <$> [ fetchgitUpdater
        , builtinsFetchGitUpdater
        , fetchTarballGithubUpdater
        , builtinsFetchTarballUpdater
        , fetchGitHubUpdater
        , hackageDirectUpdater
        ]

fetchgitUpdater :: Fetcher
fetchgitUpdater onlyCommented getComment = \case
  [matchNixLoc|
    ^fetcher {
      url = ^url;
      rev = ^rev; # rev
      sha256 = ^sha256;
      _deepClone = ^deepClone;
      _leaveDotGit = ^leaveDotGit;
      _fetchSubmodules = ^fetchSubmodules;
    }|] | extractFuncName fetcher `elem` [Just "fetchgit", Just "fetchgitPrivate"]
        , desiredRev <- commentToRequest (getComment rev)
        , onlyCommented ~> isJust desiredRev
    -> Just $ do
      url' <- fromEither $ URL <$> exprText url
      deepClone' <- fmap (fromMaybe False) . fromEither . traverse exprBool $ deepClone
      leaveDotGit' <- fmap (fromMaybe deepClone') . fromEither . traverse exprBool $ leaveDotGit
      fetchSubmodules' <- fmap (fromMaybe True) . fromEither . traverse exprBool $ fetchSubmodules
      pure $ gitUpdater url' desiredRev deepClone' leaveDotGit' fetchSubmodules' rev (Just sha256)
  _ -> Nothing

builtinsFetchGitUpdater :: Fetcher
builtinsFetchGitUpdater onlyCommented getComment = \case
  [matchNixLoc|
    ^fetcher {
      url = ^url;
      rev = ^rev; # rev
      _submodules = ^submodules;
    }|] | Just "fetchGit" <- extractFuncName fetcher
        , desiredRev <- commentToRequest (getComment rev)
        , onlyCommented ~> isJust desiredRev
    -> Just $ do
      url' <- fromEither $ URL <$> exprText url
      submodules' <- fmap (fromMaybe False) . fromEither . traverse exprBool $ submodules
      pure $ gitUpdater url' desiredRev False False submodules' rev Nothing
  _ -> Nothing

fetchTarballGithubUpdater :: Fetcher
fetchTarballGithubUpdater onlyCommented getComment = \case
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
    , onlyCommented ~> isJust comment
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
builtinsFetchTarballUpdater onlyCommented getComment = \case
  [matchNixLoc|
    ^fetcher {
      url = ^url; # [pin]
      sha256 = ^sha256;
    }|] | Just "fetchTarball" <- extractFuncName fetcher
        , comment <- getComment url
        , onlyCommented ~> isJust comment
    -> Just $ do
      url' <- fromEither $ exprText url
      pure $ tarballUpdater url' sha256
  _ -> Nothing

fetchGitHubUpdater :: Fetcher
fetchGitHubUpdater onlyCommented getComment = \case
  [matchNixLoc|
    ^fetcher {
      owner = ^owner;
      repo = ^repo;
      rev = ^rev;
      sha256 = ^sha256;
      _fetchSubmodules = ^fetchSubmodules;
    }|] | Just fun <- extractFuncName fetcher >>= \case
                        "fetchFromGitHub" -> Just GitHub
                        "fetchFromGitLab" -> Just GitLab
                        _ -> Nothing
        , desiredRev <- commentToRequest (getComment rev)
        , onlyCommented ~> isJust desiredRev
    -> Just $ do
      owner' <- fromEither $ exprText owner
      repo' <- fromEither $ exprText repo
      fetchSubmodules' <- fmap (fromMaybe False) . fromEither . traverse exprBool $ fetchSubmodules
      pure $ gitUpdater (fun owner' repo') desiredRev False False fetchSubmodules' rev (Just sha256)
  _ -> Nothing

-- |
-- @
-- callHackageDirect = {pkg, ver, sha256}:
--   let pkgver = "${pkg}-${ver}";
--   in self.callCabal2nix pkg (pkgs.fetchzip {
--        url = "mirror://hackage/${pkgver}/${pkgver}.tar.gz";
--        inherit sha256;
--      });
-- @
hackageDirectUpdater :: Fetcher
hackageDirectUpdater onlyCommented _ = \case
  [matchNixLoc|
    ^fetcher {
      pkg = ^pkg;
      ver = ^ver;
      sha256 = ^sha256;
    }
  |] | Just "callHackageDirect" <- extractFuncName fetcher
     , not onlyCommented -- no comments on this one
     -> Just $ do
      pkg' <- fromEither $ exprText pkg
      ver' <- fromEither $ exprText ver
      let pkgver = pkg' <> "-" <> ver'
          url = "mirror://hackage/" <> pkgver <> "/" <> pkgver <> ".tar.gz"
      pure $ tarballUpdater url sha256
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
  -> Bool
  -- ^ Deep Clone
  -> Bool
  -- ^ Leave .git
  -> Bool
  -- ^ Fetch submodules
  -> NExprLoc
  -- ^ rev
  -> Maybe NExprLoc
  -- ^ sha256, not present for some fetchers
  -> Updater
gitUpdater repoLocation revisionRequest deepClone leaveDotGit fetchSubmodules revExpr sha256Expr
  = Updater $ do
    let repoUrl = extractUrlString repoLocation
    logVerbose $ "Updating " <> prettyRepoLocation repoLocation
    revArgs <- case revisionRequest of
      Nothing  -> pure []
      Just req -> do
        rev <- case req of
          Pin        -> fromEither (exprText revExpr)
          DoNotPin r -> getGitFullName repoUrl r
        pure ["--rev", rev]
    let args =
          revArgs
            <> [ "--deepClone" | deepClone ]
            <> [ "--leave-dotGit" | leaveDotGit ]
            <> [ "--fetch-submodules" | fetchSubmodules ]
    o <- nixPrefetchGit args repoUrl
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

(~>) :: Bool -> Bool -> Bool
x ~> y = not x || y
