{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Update.Nix.FetchGit.Prefetch
  ( NixPrefetchGitOutput(..)
  , nixPrefetchGit
  , nixPrefetchUrl
  ) where

import           Control.Error
import           Control.Monad                  ( guard )
import           Control.Monad.IO.Class         ( liftIO )
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
  hoistEither $ case exitCode of
    ExitFailure e -> Left (NixPrefetchGitFailed e (pack nsStderr))
    ExitSuccess   -> pure ()
  decode (fromString nsStdout) ?? InvalidPrefetchGitOutput (pack nsStdout)

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
  hoistEither $ case exitCode of
    ExitFailure e -> Left (NixPrefetchUrlFailed e (pack nsStderr))
    ExitSuccess   -> pure ()
  parseSHA256 (T.strip . T.pack $ nsStdout)
    ?? InvalidPrefetchUrlOutput (pack nsStdout)

parseSHA256 :: Text -> Maybe Text
parseSHA256 t = do
  guard (base32Length == T.length t)
  guard (T.all (`elem` base32Chars) t)
  pure t
 where
  base32Chars    = "0123456789abcdfghijklmnpqrsvwxyz"
  sha256HashSize = 32
  base32Length   = (sha256HashSize * 8 - 1) `quot` 5 + 1
