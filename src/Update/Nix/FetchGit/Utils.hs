
module Update.Nix.FetchGit.Utils
  ( RepoLocation(..)
  , ourParseNixText
  , ourParseNixFile
  , extractUrlString
  , prettyRepoLocation
  , quoteString
  , extractFuncName
  , exprText
  , exprSpan
  , parseISO8601DateToDay
  , formatWarning
  , fromEither
  , note
  , refute1
  , logVerbose
  ) where

import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Control.Monad.Validate
import           Data.List.NonEmpty            as NE
import           Data.Monoid
import           Data.Text                      ( Text
                                                , splitOn
                                                , unpack
                                                )
import           Data.Time                      ( Day
                                                , defaultTimeLocale
                                                , parseTimeM
                                                )
import           Nix.Expr                hiding ( SourcePos )
import           Nix.Parser                     ( Result(..)
                                                , parseNixFileLoc
                                                , parseNixTextLoc
                                                )
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Warning
import           Update.Span

ourParseNixText :: Text -> Either Warning NExprLoc
ourParseNixText t = case parseNixTextLoc t of
  Failure parseError -> Left (CouldNotParseInput parseError)
  Success expr       -> pure expr

ourParseNixFile :: FilePath -> M NExprLoc
ourParseNixFile f = liftIO (parseNixFileLoc f) >>= \case
  Failure parseError -> refute1 (CouldNotParseInput parseError)
  Success expr       -> pure expr

-- | Get the url from either a nix expression for the url or a repo and owner
-- expression.
extractUrlString :: RepoLocation -> Text
extractUrlString = \case
  URL u -> u
  GitHub o r -> "https://github.com/" <> o <> "/" <> r <> ".git"
  GitLab o r -> "https://gitlab.com/" <> o <> "/" <> r <> ".git"

prettyRepoLocation :: RepoLocation -> Text
prettyRepoLocation = \case
  URL u      -> u
  GitHub o r -> o <> "/" <> r
  GitLab o r -> o <> "/" <> r

-- Add double quotes around a string so it can be inserted into a Nix
-- file as a string literal.
quoteString :: Text -> Text
quoteString t = "\"" <> t <> "\""

-- | Get the string value of a particular expression, returns a 'Warning' if
-- the expression is not a string value.
--
-- TODO: Use 'evalExpr' here
exprText :: NExprLoc -> Either Warning Text
exprText = \case
  (AnnE _ (NStr (DoubleQuoted [Plain t]))) -> pure t
  e -> Left (NotAString e)

-- | Get the 'SrcSpan' covering a particular expression.
exprSpan :: NExprLoc -> SrcSpan
exprSpan (AnnE s _) = s
exprSpan _ = error "unreachable" -- TODO: Add pattern completeness to hnix

-- | Given an expression that is supposed to represent a function,
-- extracts the name of the function.  If we cannot figure out the
-- function name, returns Nothing.
extractFuncName :: NExprLoc -> Maybe Text
extractFuncName (AnnE _ (NSym name)) = Just name
extractFuncName (AnnE _ (NSelect _ (NE.last -> StaticKey name) _)) = Just name
extractFuncName _ = Nothing

-- Takes an ISO 8601 date and returns just the day portion.
parseISO8601DateToDay :: Text -> Either Warning Day
parseISO8601DateToDay t =
  let justDate = (unpack . Prelude.head . splitOn "T") t
  in  maybe (Left $ InvalidDateString t)
            Right
            (parseTimeM False defaultTimeLocale "%Y-%m-%d" justDate)

formatWarning :: Warning -> String
formatWarning (CouldNotParseInput doc) = show doc
formatWarning (MissingAttr attrName) =
  "Error: The \"" <> unpack attrName <> "\" attribute is missing."
formatWarning (DuplicateAttrs attrName) =
  "Error: The \"" <> unpack attrName <> "\" attribute appears twice in a set."
formatWarning (NotAString expr) =
  "Error: The expression at "
  <> (prettyPrintSourcePos . spanBegin . exprSpan) expr
  <> " is not a string literal."
formatWarning (NixPrefetchGitFailed exitCode errorOutput) =
  "Error: nix-prefetch-git failed with exit code " <> show exitCode
  <> " and error output:\n" <> unpack errorOutput
formatWarning (InvalidPrefetchGitOutput output) =
  "Error: Output from nix-prefetch-git is invalid:\n" <> show output
formatWarning (NixPrefetchUrlFailed exitCode errorOutput) =
  "Error: nix-prefetch-url failed with exit code " <> show exitCode
  <> " and error output:\n" <> unpack errorOutput
formatWarning (InvalidPrefetchUrlOutput output) =
  "Error: Output from nix-prefetch-url is invalid:\n" <> show output
formatWarning (InvalidDateString text) =
  "Error: Date string is invalid: " <> show text
formatWarning (GitLsRemoteFailed exitCode errorOutput) =
  "Error: git ls-remote failed with exit code " <> show exitCode
  <> " and error output:\n" <> unpack errorOutput
formatWarning (NoSuchRef text) =
  "Error: No such ref: " <> show text
formatWarning (InvalidGitLsRemoteOutput output) =
  "Error: Output from git ls-remote is invalid:\n" <> show output

----------------------------------------------------------------
-- Errors
----------------------------------------------------------------

fromEither :: Either Warning a -> M a
fromEither = \case
  Left  e -> refute1 e
  Right a -> pure a

note :: Warning -> Maybe a -> M a
note e = \case
  Nothing -> refute1 e
  Just a -> pure a

refute1 :: Warning -> M a
refute1 = refute . Dual . pure

----------------------------------------------------------------
-- Logging
----------------------------------------------------------------

logVerbose :: Text -> M ()
logVerbose t = do
  Env{..} <- ask
  sayLog Verbose t
