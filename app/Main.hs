{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Control.Category               ( (>>>) )
import           Data.Either                    ( partitionEithers )
import           Data.Foldable
import qualified Data.Text.IO                  as T
import           Data.Version                   ( showVersion )
import           Options.Applicative
import           Options.Generic
import           Paths_update_nix_fetchgit      ( version )
import           Say
import           Text.ParserCombinators.ReadP   ( char
                                                , eof
                                                , readP_to_S
                                                , readS_to_P
                                                , sepBy
                                                )
import           Text.Regex.TDFA
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
env Options {..} =
  let sayLog
        | verbose = const sayErr
        | quiet = \case
          Verbose -> const (pure ())
          Normal  -> const (pure ())
          Quiet   -> sayErr
        | otherwise = \case
          Verbose -> const (pure ())
          Normal  -> sayErr
          Quiet   -> sayErr
      updateLocations = [ (l, c) | Position l c <- location ]
      attrPatterns = attribute
  in Env { .. }

----------------------------------------------------------------
-- Options
----------------------------------------------------------------

data Options w = Options
  { verbose  :: w ::: Bool <!> "False"
  , quiet    :: w ::: Bool <!> "False"
  , location :: w ::: [Position] <?> "Source location to limit updates to, Combined using inclusive or"
  , attribute
      :: w ::: [Regex] <?> "Pattern (POSIX regex) to limit updates to expressions under matching names in attrsets and let bindings. Combined using inclusing or, if this isn't specified then no expressions will be filtered by attribute name"
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
    <*> (   (,)
        <$> (unwrap <$> parseRecordWithModifiers defaultModifiers
              { shortNameModifier = firstLetter
              }
            )
        <*> many
              (strArgument
                (  help "Nix files to update"
                <> Options.Applicative.metavar "FILE"
                )
              )
        )
 where
  versionString = "update-nix-fetchgit-" <> showVersion version
  versionOption :: Parser (a -> a)
  versionOption = infoOption
    versionString
    (long "version" <> help ("print " <> versionString))

instance ParseRecord (Options Wrapped)

data Position = Position Int Int
  deriving Show

instance Read Position where
  readsPrec _ = readP_to_S $ do
    [line, col] <- sepBy (readS_to_P reads) (char ':')
    eof
    pure $ Position line col

instance ParseField Position where
  metavar _ = "LINE:COL"

instance Read Regex where
  readsPrec _ s = case makeRegexM s of
    Nothing -> []
    Just r  -> [(r, "")]

instance ParseField Regex where
  metavar _ = "REGEX"
  readField = eitherReader makeRegexM

instance (e ~ String) => MonadFail (Either e) where
  fail = Left

note :: a -> Maybe b -> Either a b
note e = maybe (Left e) Right

collectErrors :: [Either e a] -> Either [e] [a]
collectErrors = partitionEithers >>> \case
  ([], as) -> Right as
  (es, _ ) -> Left es
