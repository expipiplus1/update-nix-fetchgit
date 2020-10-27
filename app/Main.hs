
module Main where

import           Data.Foldable
import qualified Data.Text.IO                  as T
import           Options.Applicative
import           Options.Generic         hiding ( metavar )
import           Update.Nix.FetchGit

main :: IO ()
main = do
  (Options {..}, fs) <- parseOpts
  let goStd = T.putStr =<< processText =<< T.getContents
  case fs of
    [] -> goStd
    _  -> for_ fs $ \f -> if f == "-" then goStd else processFile f

----------------------------------------------------------------
-- Options
----------------------------------------------------------------

newtype Options w = Options
  { verbose :: w ::: Bool <!> "False"
  }
  deriving stock (Generic)

parseOpts :: IO (Options Unwrapped, [FilePath])
parseOpts = customExecParser (prefs $ multiSuffix "...")
                             (info optParser (progDesc desc))
 where
  desc = unlines
    [ "Update fetchers in Nix expressions."
    , "Without any files stdin and stdout will be used"
    ]

optParser :: Parser (Options Unwrapped, [FilePath])
optParser = (,) <$> (unwrap <$> parseRecord) <*> many
  (strArgument (help "Nix files to update" <> metavar "FILE"))

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)
