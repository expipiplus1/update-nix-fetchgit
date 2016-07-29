{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Update.Nix.FetchGit.Prefetch
  ( NixPrefetchGitOutput(..)
  , nixPrefetchGit
  ) where

import           Data.Aeson                  (FromJSON, decode)
import           Data.ByteString.Lazy.UTF8   (fromString)
import           Data.Text
import           GHC.Generics
import           System.Exit                 (ExitCode (..))
import           System.IO                   (hPutStrLn, stderr)
import           System.Process              (readProcessWithExitCode)
import           Update.Nix.FetchGit.Warning


-- | The type of nix-prefetch-git's output
data NixPrefetchGitOutput = NixPrefetchGitOutput{ url    :: Text
                                                , rev    :: Text
                                                , sha256 :: Text
                                                , date   :: Text
                                                }
  deriving (Show, Generic, FromJSON)

-- | Run nix-prefetch-git
nixPrefetchGit :: Text -- ^ The URL to prefetch
               -> IO (Either Warning NixPrefetchGitOutput)
nixPrefetchGit prefetchURL = do
  (exitCode, nsStdout, nsStderr) <-
    readProcessWithExitCode "nix-prefetch-git" [unpack prefetchURL] ""
  hPutStrLn stderr nsStderr
  pure $ case exitCode of
    ExitFailure e -> Left (NixPrefetchGitFailed e)
    ExitSuccess ->
      case decode (fromString nsStdout) of
        Nothing -> Left (InvalidPrefetchGitOutput (pack nsStdout))
        Just o -> Right o
