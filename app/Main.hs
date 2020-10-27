
module Main where

import           Data.Foldable
import qualified Data.Text.IO                  as T
import           Data.Version                   ( showVersion )
import           Options.Applicative
import           Options.Generic         hiding ( metavar )
import           Paths_update_nix_fetchgit      ( version )
import           Say
import           Update.Nix.FetchGit
import           Update.Nix.FetchGit.Types

main :: IO ()
main = do
  (o, fs) <- parseOpts
  let e     = env o
  let goStd = T.putStr =<< processText e =<< T.getContents
  case fs of
    [] -> goStd
    _  -> for_ fs $ \f -> if f == "-" then goStd else processFile e f

----------------------------------------------------------------
-- Env
----------------------------------------------------------------

env :: Options Unwrapped -> Env
env Options {..} = Env $ if verbose
  then const sayErr
  else if quiet
    then \case
      Verbose -> const (pure ())
      Normal  -> const (pure ())
      Quiet   -> sayErr
    else \case
      Verbose -> const (pure ())
      Normal  -> sayErr
      Quiet   -> sayErr

----------------------------------------------------------------
-- Options
----------------------------------------------------------------

data Options w = Options
  { verbose :: w ::: Bool <!> "False"
  , quiet   :: w ::: Bool <!> "False"
  }
  deriving stock Generic

parseOpts :: IO (Options Unwrapped, [FilePath])
parseOpts = customExecParser (prefs $ multiSuffix "...")
                             (info optParser (progDesc desc))
 where
  desc = unlines
    [ "Update fetchers in Nix expressions."
    , "Without any files, stdin and stdout will be used"
    ]

optParser :: Parser (Options Unwrapped, [FilePath])
optParser =
  versionOption
    <*> ((,) <$> (unwrap <$> parseRecord) <*> many
          (strArgument (help "Nix files to update" <> metavar "FILE"))
        )
 where
  versionString = "update-nix-fetchgit-" <> showVersion version
  versionOption :: Parser (a -> a)
  versionOption = infoOption
    versionString
    (long "version" <> help ("print " <> versionString))

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)
