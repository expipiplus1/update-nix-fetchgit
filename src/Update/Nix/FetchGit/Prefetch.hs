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
                                                }
  deriving (Show, Generic, FromJSON)

-- | Run nix-prefetch-git
nixPrefetchGit :: Text -- ^ The URL to prefetch
               -> IO (Either Warning NixPrefetchGitOutput)
nixPrefetchGit prefetchURL = do
  let nixPrefetchCommand = "nix-prefetch-git " ++ unpack prefetchURL
      nixShellArgs = [ "-p", "nix-prefetch-git"
                     , "-p", "nix"
                     , "--command", nixPrefetchCommand
                     ]
      nsStdin = ""
  (exitCode, nsStdout, nsStderr) <-
    readProcessWithExitCode "nix-shell" nixShellArgs nsStdin
  hPutStrLn stderr nsStderr
  pure $ case exitCode of
    ExitSuccess   ->
      case decode (fromString nsStdout) of
        Nothing -> Left (InvalidPrefetchGitOutput (pack nsStdout))
        Just o  -> Right o
    ExitFailure e -> Left (NixShellFailed e)
