{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Update.Nix.FetchGit.Prefetch
  ( NixPrefetchGitOutput(..)
  , nixPrefetchGit
  ) where

import           Control.Error
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, decode)
import           Data.ByteString.Lazy.UTF8   (fromString)
import           Data.Text                   (Text, pack, unpack)
import           GHC.Generics
import           System.Exit                 (ExitCode (..))
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
nixPrefetchGit :: [Text] -- ^ Extra arguments for nix-prefetch-git
               -> Text   -- ^ The URL to prefetch
               -> IO (Either Warning NixPrefetchGitOutput)
nixPrefetchGit extraArgs prefetchURL = runExceptT $ do
  (exitCode, nsStdout, nsStderr) <- liftIO $
    readProcessWithExitCode "nix-prefetch-git" (map unpack extraArgs ++ [unpack prefetchURL]) ""
  hoistEither $ case exitCode of
    ExitFailure e -> Left (NixPrefetchGitFailed e (pack nsStderr))
    ExitSuccess -> pure ()
  decode (fromString nsStdout) ?? InvalidPrefetchGitOutput (pack nsStdout)
