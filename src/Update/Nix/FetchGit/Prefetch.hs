module Update.Nix.FetchGit.Prefetch
  ( NixPrefetchGitOutput(..)
  , nixPrefetchGit
  , nixPrefetchUrl
  , getGitFullName
  , getGitRevision
  , getGitHubRevisionDate
  , Revision(..)
  ) where

import           Control.Monad                  ( guard )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Aeson                     ( FromJSON
                                                , decode
                                                )
import           Data.ByteString.Lazy.UTF8      ( fromString )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as T
import           Data.Time                      ( Day )
import           GHC.Generics
import           GitHub.REST
import           System.Exit                    ( ExitCode(..) )
import           System.Process                 ( readProcessWithExitCode )
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Utils
import           Update.Nix.FetchGit.Warning


-- | The type of nix-prefetch-git's output
data NixPrefetchGitOutput = NixPrefetchGitOutput
  { url    :: Text
  , rev    :: Text
  , sha256 :: Text
  , date   :: Text
  }
  deriving (Show, Generic, FromJSON)

-- | Run nix-prefetch-git
nixPrefetchGit
  :: [Text] -- ^ Extra arguments for nix-prefetch-git
  -> Text   -- ^ The URL to prefetch
  -> M NixPrefetchGitOutput
nixPrefetchGit extraArgs prefetchURL = do
  (exitCode, nsStdout, nsStderr) <- liftIO $ readProcessWithExitCode
    "nix-prefetch-git"
    (map unpack extraArgs ++ [unpack prefetchURL])
    ""
  case exitCode of
    ExitFailure e -> refute1 (NixPrefetchGitFailed e (pack nsStderr))
    ExitSuccess   -> pure ()
  note (InvalidPrefetchGitOutput (pack nsStdout)) (decode (fromString nsStdout))

-- | Run nix-prefetch-url
nixPrefetchUrl
  :: [Text] -- ^ Extra arguments for nix-prefetch-url
  -> Text   -- ^ The URL to prefetch
  -> M Text -- The sha256 output
nixPrefetchUrl extraArgs prefetchURL = do
  (exitCode, nsStdout, nsStderr) <- liftIO $ readProcessWithExitCode
    "nix-prefetch-url"
    (map unpack extraArgs ++ [unpack prefetchURL])
    ""
  case exitCode of
    ExitFailure e -> refute1 (NixPrefetchUrlFailed e (pack nsStderr))
    ExitSuccess   -> pure ()
  note (InvalidPrefetchUrlOutput (pack nsStdout))
       (parseSHA256 (T.strip . T.pack $ nsStdout))

newtype Revision = Revision { unRevision :: Text }

-- | Discover if this ref is a branch or a tag
--
-- >>> runM _ $ getGitFullName "https://github.com/expipiplus1/update-nix-fetchgit" (Revision "0.1.0.0")
-- Right "refs/tags/0.1.0.0"
--
-- >>> runM _ $ getGitFullName "https://github.com/expipiplus1/update-nix-fetchgit" (Revision "joe-fetchTarball")
-- Right "refs/heads/joe-fetchTarball"
getGitFullName
  :: Text
  -- ^ git repo location
  -> Revision
  -- ^ branch or tag name
  -> M Text
  -- ^ Full name, i.e. with refs/heads/ or refs/tags/
getGitFullName repo revision = do
  gitLsRemotes repo revision >>= \case
    Just (_hash, name) -> pure name
    Nothing            -> refute1 $ NoSuchRef (unRevision revision)

-- | Return a tag or a hash
getGitRevision
  :: Text
  -- ^ git repo location
  -> Revision
  -- ^ branch or tag name
  -> M Text
getGitRevision repo revision = do
  gitLsRemotes repo revision >>= \case
    Just (hash, name) | Just tag <- stripPrefix "refs/tags/" name -> pure tag
                      | otherwise -> pure hash
    Nothing -> refute1 $ NoSuchRef (unRevision revision)

-- | Run git ls-remote --heads --tags --sort=-v:refname and return the first
-- match if any. Use '--heads --tags' if the revision doesn't start with
-- 'refs/' to avoid getting 'remote' refs.
gitLsRemotes :: Text -> Revision -> M (Maybe (Text, Text))
gitLsRemotes repo revision = do
  let headsTags = if T.isPrefixOf "refs/" (unRevision revision)
        then []
        else ["--heads", "--tags"]
      args =
        ["ls-remote", "--sort=-v:refname", repo, unRevision revision]
          <> headsTags :: [Text]
  logVerbose $ "Calling: git " <> T.unwords args
  (exitCode, nsStdout, nsStderr) <- liftIO
    $ readProcessWithExitCode "git" (T.unpack <$> args) ""
  case exitCode of
    ExitFailure e -> refute1 (NixPrefetchGitFailed e (pack nsStderr))
    ExitSuccess   -> pure ()
  let stdoutText = T.pack nsStdout
  case fmap T.words . T.lines $ stdoutText of
    []               -> pure Nothing
    [hash, name] : _ -> pure $ Just (hash, name)
    _                -> refute1 (InvalidGitLsRemoteOutput stdoutText)

getGitHubRevisionDate :: Text -> Text -> Revision -> M Day
getGitHubRevisionDate owner repo revision = do
  dateString <- runGitHubT ghState $ do
    ref <- queryGitHub GHEndpoint
      { method       = GET
      , endpoint     = "/repos/:owner/:repo/commits/:ref"
      , endpointVals = [ "owner" := owner
                       , "repo" := repo
                       , "ref" := unRevision revision
                       ]
      , ghData       = []
      }
    pure $ ref .: "commit" .: "committer" .: "date"
  fromEither $ parseISO8601DateToDay dateString

ghState :: GitHubSettings
ghState = GitHubSettings { token      = Nothing
                         , userAgent  = "expipiplus1/update-nix-fetchgit"
                         , apiVersion = "v3"
                         }

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

parseSHA256 :: Text -> Maybe Text
parseSHA256 t = do
  guard (base32Length == T.length t)
  guard (T.all (`elem` base32Chars) t)
  pure t
 where
  base32Chars    = "0123456789abcdfghijklmnpqrsvwxyz" :: String
  sha256HashSize = 32
  base32Length   = (sha256HashSize * 8 - 1) `quot` 5 + 1

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p t =
  if p `T.isPrefixOf` t then Just $ T.drop (T.length p) t else Nothing
