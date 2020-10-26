{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}

module Update.Nix.FetchGit.Prefetch
  ( NixPrefetchGitOutput(..)
  , nixPrefetchGit
  , nixPrefetchUrl
  , getGitFullName
  ) where

import           Control.Monad.Except
import           Data.Aeson                     ( FromJSON
                                                , decode
                                                )
import           Data.ByteString.Lazy.UTF8      ( fromString )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as T
import           GHC.Generics
import           System.Exit                    ( ExitCode(..) )
import           System.Process                 ( readProcessWithExitCode )
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
  -> IO (Either Warning NixPrefetchGitOutput)
nixPrefetchGit extraArgs prefetchURL = runExceptT $ do
  (exitCode, nsStdout, nsStderr) <- liftIO $ readProcessWithExitCode
    "nix-prefetch-git"
    (map unpack extraArgs ++ [unpack prefetchURL])
    ""
  case exitCode of
    ExitFailure e -> throwError (NixPrefetchGitFailed e (pack nsStderr))
    ExitSuccess   -> pure ()
  note (InvalidPrefetchGitOutput (pack nsStdout)) (decode (fromString nsStdout))

-- | Run nix-prefetch-url --unpack
nixPrefetchUrl
  :: [Text] -- ^ Extra arguments for nix-prefetch-url
  -> Text   -- ^ The URL to prefetch
  -> IO (Either Warning Text) -- The sha256 output
nixPrefetchUrl extraArgs prefetchURL = runExceptT $ do
  (exitCode, nsStdout, nsStderr) <- liftIO $ readProcessWithExitCode
    "nix-prefetch-url"
    ("--unpack" : map unpack extraArgs ++ [unpack prefetchURL])
    ""
  case exitCode of
    ExitFailure e -> throwError (NixPrefetchUrlFailed e (pack nsStderr))
    ExitSuccess   -> pure ()
  note (InvalidPrefetchUrlOutput (pack nsStdout))
       (parseSHA256 (T.strip . T.pack $ nsStdout))

-- | Discover if this ref is a branch or a tag
--
-- >>> runExceptT $ getGitFullName "https://github.com/expipiplus1/update-nix-fetchgit" "0.1.0.0"
-- Right "refs/tags/0.1.0.0"
--
-- >>> runExceptT $ getGitFullName "https://github.com/expipiplus1/update-nix-fetchgit" "joe-fetchTarball"
-- Right "refs/heads/joe-fetchTarball"
getGitFullName
  :: Text -- ^ git repo location
  -> Text -- ^ branch or tag name
  -> ExceptT Warning IO Text
  -- ^ Full name, i.e. with refs/heads/ or refs/tags/
getGitFullName repo ref = do
  (exitCode, nsStdout, nsStderr) <- liftIO $ readProcessWithExitCode
    "git"
    ["ls-remote", T.unpack repo, T.unpack ref]
    ""
  case exitCode of
    ExitFailure e -> throwError (NixPrefetchGitFailed e (pack nsStderr))
    ExitSuccess   -> pure ()
  let stdoutText = T.pack nsStdout
  case fmap T.words . T.lines $ stdoutText of
    []                -> throwError (NoSuchRef ref)
    [_hash, name] : _ -> pure name
    _                 -> throwError $ InvalidGitLsRemoteOutput stdoutText

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

note :: Monad m => e -> Maybe a -> ExceptT e m a
note e = maybe (throwError e) pure
