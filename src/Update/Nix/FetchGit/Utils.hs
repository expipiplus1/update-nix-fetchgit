
module Update.Nix.FetchGit.Utils
  ( RepoLocation(..)
  , ourParseNixText
  , ourParseNixFile
  , extractUrlString
  , prettyRepoLocation
  , quoteString
  , extractFuncName
  , exprText
  , exprBool
  , exprSpan
  , containsPosition
  , parseISO8601DateToDay
  , formatWarning
  , fromEither
  , note
  , refute1
  , logVerbose
  , logNormal
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
import qualified Data.Text as T
import Nix.Atoms (NAtom(NBool))
import Data.Fix

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

exprBool :: NExprLoc -> Either Warning Bool
exprBool = \case
  (AnnE _ (NConstant (NBool b))) -> pure b
  e                              -> Left (NotABool e)

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

formatWarning :: Warning -> Text
formatWarning (CouldNotParseInput doc) = tShow doc
formatWarning (MissingAttr attrName) =
  "Error: The \"" <> attrName <> "\" attribute is missing."
formatWarning (DuplicateAttrs attrName) =
  "Error: The \"" <> attrName <> "\" attribute appears twice in a set."
formatWarning (NotAString expr) =
  "Error: The expression at "
    <> (T.pack . prettyPrintSourcePos . spanBegin . exprSpan) expr
    <> " is not a string literal."
formatWarning (NotABool expr) =
  "Error: The expression at "
    <> (T.pack . prettyPrintSourcePos . spanBegin . exprSpan) expr
    <> " is not a boolean literal."
formatWarning (NixPrefetchGitFailed exitCode errorOutput) =
  "Error: nix-prefetch-git failed with exit code "
    <> tShow exitCode
    <> " and error output:\n"
    <> errorOutput
formatWarning (InvalidPrefetchGitOutput output) =
  "Error: Output from nix-prefetch-git is invalid:\n" <> tShow output
formatWarning (NixPrefetchUrlFailed exitCode errorOutput) =
  "Error: nix-prefetch-url failed with exit code "
    <> tShow exitCode
    <> " and error output:\n"
    <> errorOutput
formatWarning (InvalidPrefetchUrlOutput output) =
  "Error: Output from nix-prefetch-url is invalid:\n" <> tShow output
formatWarning (InvalidDateString text) =
  "Error: Date string is invalid: " <> tShow text
formatWarning (GitLsRemoteFailed exitCode errorOutput) =
  "Error: git ls-remote failed with exit code "
    <> tShow exitCode
    <> " and error output:\n"
    <> errorOutput
formatWarning (NoSuchRef text) = "Error: No such ref: " <> tShow text
formatWarning (InvalidGitLsRemoteOutput output) =
  "Error: Output from git ls-remote is invalid:\n" <> tShow output

tShow :: Show a => a -> Text
tShow = T.pack . show

----------------------------------------------------------------
-- Locations
----------------------------------------------------------------

containsPosition :: NExprLoc -> (Int, Int) -> Bool
containsPosition (Fix (Compose (Ann (SrcSpan begin end) _))) p =
  let unSourcePos (SourcePos _ l c) = (unPos l, unPos c)
  in  p >= unSourcePos begin && p < unSourcePos end

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
  liftIO $ sayLog Verbose t

logNormal :: Text -> M ()
logNormal t = do
  Env {..} <- ask
  liftIO $ sayLog Normal t
