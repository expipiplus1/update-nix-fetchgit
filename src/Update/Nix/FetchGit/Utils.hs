module Update.Nix.FetchGit.Utils
  ( RepoLocation (..),
    ourParseNixText,
    ourParseNixFile,
    extractUrlString,
    prettyRepoLocation,
    quoteString,
    extractFuncName,
    pathText,
    exprText,
    exprBool,
    exprSpan,
    containsPosition,
    parseISO8601DateToDay,
    formatWarning,
    fromEither,
    note,
    refute1,
    logVerbose,
    logNormal,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Validate
import Data.Fix
import Data.List.NonEmpty as NE
import Data.Monoid
import Data.Text
  ( Text,
    splitOn,
    unpack,
  )
import qualified Data.Text as T
import Data.Time
  ( Day,
    defaultTimeLocale,
    parseTimeM,
  )
import Nix.Atoms (NAtom (NBool))
import Nix.Expr hiding (SourcePos)
import Nix.Parser
  ( parseNixFileLoc,
    parseNixTextLoc,
  )
import Nix.Utils (Path (..))
import Update.Nix.FetchGit.Types
import Update.Nix.FetchGit.Warning
import Update.Span

ourParseNixText :: Text -> Either Warning NExprLoc
ourParseNixText t = case parseNixTextLoc t of
  Left parseError -> Left (CouldNotParseInput (tShow parseError))
  Right expr -> pure expr

ourParseNixFile :: FilePath -> M NExprLoc
ourParseNixFile f =
  liftIO (parseNixFileLoc (Path f)) >>= \case
    Left parseError -> refute1 (CouldNotParseInput (tShow parseError))
    Right expr -> pure expr

-- | Get the url from either a nix expression for the url or a repo and owner
-- expression.
extractUrlString :: RepoLocation -> Text
extractUrlString = \case
  URL u -> u
  GitHub o r -> "https://github.com/" <> o <> "/" <> r <> ".git"
  GitLab o r -> "https://gitlab.com/" <> o <> "/" <> r <> ".git"

prettyRepoLocation :: RepoLocation -> Text
prettyRepoLocation = \case
  URL u -> u
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
  (Ann _ (NStr (DoubleQuoted [Plain t]))) -> pure t
  e -> Left (NotAString e)

exprBool :: NExprLoc -> Either Warning Bool
exprBool = \case
  (Ann _ (NConstant (NBool b))) -> pure b
  e -> Left (NotABool e)

-- | Get the 'SrcSpan' covering a particular expression.
exprSpan :: NExprLoc -> SrcSpan
exprSpan (Ann s _) = s

-- | Given an expression that is supposed to represent a function,
-- extracts the name of the function.  If we cannot figure out the
-- function name, returns Nothing.
extractFuncName :: NExprLoc -> Maybe VarName
extractFuncName (Ann _ (NSym name)) = Just name
extractFuncName (Ann _ (NSelect _ _ (NE.last -> StaticKey name))) = Just name
extractFuncName _ = Nothing

pathText :: NAttrPath r -> Maybe Text
pathText = fmap (T.concat . toList) . traverse e
  where
    e :: NKeyName r -> Maybe Text
    e = \case
      StaticKey (VarName s) -> Just s
      DynamicKey (Plain s) -> t s
      DynamicKey EscapedNewline -> Just "\n"
      DynamicKey (Antiquoted _) -> Nothing
    t :: NString r -> Maybe Text
    t =
      fmap T.concat
        . traverse a
        . ( \case
              DoubleQuoted as -> as
              Indented _ as -> as
          )
    a :: Antiquoted Text r -> Maybe Text
    a = \case
      Plain s -> pure s
      EscapedNewline -> pure "\n"
      Antiquoted _ -> Nothing

-- Takes an ISO 8601 date and returns just the day portion.
parseISO8601DateToDay :: Text -> Either Warning Day
parseISO8601DateToDay t =
  let justDate = (unpack . Prelude.head . splitOn "T") t
   in maybe
        (Left $ InvalidDateString t)
        Right
        (parseTimeM False defaultTimeLocale "%Y-%m-%d" justDate)

formatWarning :: Warning -> Text
formatWarning (CouldNotParseInput doc) = doc
formatWarning (MissingAttr attrName) =
  "Error: The \"" <> attrName <> "\" attribute is missing."
formatWarning (DuplicateAttrs attrName) =
  "Error: The \"" <> attrName <> "\" attribute appears twice in a set."
formatWarning (NotAString expr) =
  "Error: The expression at "
    <> (T.pack . prettyPrintSourcePos . getSpanBegin . exprSpan) expr
    <> " is not a string literal."
formatWarning (NotABool expr) =
  "Error: The expression at "
    <> (T.pack . prettyPrintSourcePos . getSpanBegin . exprSpan) expr
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
containsPosition (Fix (Compose (AnnUnit (SrcSpan begin end) _))) p =
  let unSourcePos (NSourcePos _ (NPos l) (NPos c)) = (unPos l, unPos c)
   in p >= unSourcePos begin && p < unSourcePos end

----------------------------------------------------------------
-- Errors
----------------------------------------------------------------

fromEither :: Either Warning a -> M a
fromEither = \case
  Left e -> refute1 e
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
  Env {..} <- ask
  liftIO $ sayLog Verbose t

logNormal :: Text -> M ()
logNormal t = do
  Env {..} <- ask
  liftIO $ sayLog Normal t
