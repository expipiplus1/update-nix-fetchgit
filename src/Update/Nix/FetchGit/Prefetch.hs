
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
data NixPrefetchGitOutput = NixPrefetchGitOutput{ url    :: Text
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

-- | Run nix-prefetch-url --unpack
nixPrefetchUrl
  :: [Text] -- ^ Extra arguments for nix-prefetch-url
  -> Text   -- ^ The URL to prefetch
  -> M Text -- The sha256 output
nixPrefetchUrl extraArgs prefetchURL = do
  (exitCode, nsStdout, nsStderr) <- liftIO $ readProcessWithExitCode
    "nix-prefetch-url"
    ("--unpack" : map unpack extraArgs ++ [unpack prefetchURL])
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
  (stdoutText, rs) <- gitLsRemotes repo revision
  case rs of
    [_hash, name] : _ -> pure name
    _                 -> refute1 $ InvalidGitLsRemoteOutput stdoutText

-- | Return a tag or a hash
getGitRevision
  :: Text
  -- ^ git repo location
  -> Revision
  -- ^ branch or tag name
  -> M Text
  -- ^ Full name, i.e. with refs/heads/ or refs/tags/
getGitRevision repo revision = do
  (stdoutText, rs) <- gitLsRemotes repo revision
  case rs of
    [hash, name] : _ | Just tag <- stripPrefix "refs/tags/" name -> pure tag
                     | otherwise -> pure hash
    _ -> refute1 $ InvalidGitLsRemoteOutput stdoutText

gitLsRemotes :: Text -> Revision -> M (Text, [[Text]])
gitLsRemotes repo revision = do
  (exitCode, nsStdout, nsStderr) <- liftIO $ readProcessWithExitCode
    "git"
    ["ls-remote", T.unpack repo, T.unpack (unRevision revision)]
    ""
  case exitCode of
    ExitFailure e -> refute1 (NixPrefetchGitFailed e (pack nsStderr))
    ExitSuccess   -> pure ()
  let stdoutText = T.pack nsStdout
  case fmap T.words . T.lines $ stdoutText of
    [] -> refute1 (NoSuchRef (unRevision revision))
    rs -> pure (stdoutText, rs)

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

ghState :: GitHubState
ghState = GitHubState { token      = Nothing
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
stripPrefix p t = if p `T.isPrefixOf` t
                    then Just $ T.drop (T.length p) t
                    else Nothing
