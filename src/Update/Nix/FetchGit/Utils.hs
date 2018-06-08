{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Update.Nix.FetchGit.Utils
  ( RepoLocation(..)
  , ourParseNixFile
  , extractUrlString
  , quoteString
  , extractFuncName
  , extractAttr
  , findAttr
  , exprText
  , exprSpan
  , parseISO8601DateToDay
  , formatWarning
  ) where

import           Data.Generics.Uniplate.Data (transform)
import           Data.Maybe                  (catMaybes)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text, unpack, splitOn)
import           Data.Time                   (parseTimeM, defaultTimeLocale)
import qualified Data.List.NonEmpty as NE
import           Nix.Parser                  (parseNixFileLoc, Result(..))
import           Nix.Expr hiding(SourcePos)
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Warning
import           Update.Span
import qualified Text.Megaparsec.Pos as M

ourParseNixFile :: FilePath -> IO (Either Warning NExprLoc)
ourParseNixFile f =
  parseNixFileLoc f >>= \case
    Failure parseError -> pure $ Left (CouldNotParseInput parseError)
    Success expr -> pure $ pure $ fixNixSets expr

-- Convert all NRecSet values (recursive sets) to NSet values because
-- we do not care about the distinction between NRecSet and NSet and
-- we want our program to treat both types equally.
fixNixSets :: NExprLoc -> NExprLoc
fixNixSets = transform fix
  where fix (AnnE s (NRecSet bindings)) = AnnE s (NSet bindings)
        fix n = n

-- | Get the url from either a nix expression for the url or a repo and owner
-- expression.
extractUrlString :: RepoLocation -> Text
extractUrlString = \case
  URL u -> u
  GitHub o r -> "https://github.com/" <> o <> "/" <> r <> ".git"

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

-- | Get the 'SourceSpan' covering a particular expression.
exprSpan :: NExprLoc -> SourceSpan
exprSpan expr = SourceSpan (convertSourcePos begin) (convertSourcePos end)
         where (AnnE (SrcSpan begin end) _) = expr

-- | Go from a 'Delta' to a 'SourcePos'.
convertSourcePos :: M.SourcePos -> SourcePos
convertSourcePos M.SourcePos { sourceLine = l, sourceColumn = c } = SourcePos
  (pred $ fromPos l)
  (pred $ fromPos c)
  where fromPos = fromIntegral . M.unPos

-- | Given an expression that is supposed to represent a function,
-- extracts the name of the function.  If we cannot figure out the
-- function name, returns Nothing.
extractFuncName :: NExprLoc -> Maybe Text
extractFuncName = \case
  AnnE _ (NSym name) -> Just name
  AnnE _ (NSelect _ (NE.last -> StaticKey name) _) -> Just name
  _                  -> Nothing

-- | Extract a named attribute from an attrset.
extractAttr :: Text -> [Binding a] -> Either Warning a
extractAttr name bs = case catMaybes (matchAttr name <$> bs) of
  [x] -> pure x
  []  -> Left (MissingAttr name)
  _   -> Left (DuplicateAttrs name)

-- | Find a named attribute in an attrset.  This is appropriate for
-- the case when a missing attribute is not an error.
findAttr :: Text -> [Binding a] -> Either Warning (Maybe a)
findAttr name bs = case catMaybes (matchAttr name <$> bs) of
  [x] -> pure (Just x)
  []  -> pure Nothing
  _   -> Left (DuplicateAttrs name)

-- | Returns 'Just value' if this attribute's key matches the text, otherwise
-- Nothing.
matchAttr :: Text -> Binding a -> Maybe a
matchAttr t = \case
  NamedVar (StaticKey t' NE.:| []) x _ | t == t' -> Just x
  NamedVar{}                            -> Nothing
  Inherit{}                             -> Nothing

-- Takes an ISO 8601 date and returns just the day portion.
parseISO8601DateToDay :: Text -> Either Warning Day
parseISO8601DateToDay t =
  let justDate = (unpack . Prelude.head . splitOn "T") t in
  case parseTimeM False defaultTimeLocale "%Y-%m-%d" justDate of
    Nothing -> Left $ InvalidDateString t
    Just day -> pure day

formatWarning :: Warning -> String
formatWarning (CouldNotParseInput doc) = show doc
formatWarning (MissingAttr attrName) =
  "Error: The \"" <> unpack attrName <> "\" attribute is missing."
formatWarning (DuplicateAttrs attrName) =
  "Error: The \"" <> unpack attrName <> "\" attribute appears twice in a set."
formatWarning (NotAString expr) =
  "Error: The expression at "
  <> (prettyPrintSourcePos . sourceSpanBegin . exprSpan) expr
  <> " is not a string literal."
formatWarning (NixPrefetchGitFailed exitCode errorOutput) =
  "Error: nix-prefetch-git failed with exit code " <> show exitCode
  <> " and error output:\n" <> unpack errorOutput
formatWarning (InvalidPrefetchGitOutput output) =
  "Error: Output from nix-prefetch-git is invalid:\n" <> show output
formatWarning (InvalidDateString text) =
  "Error: Date string is invalid: " <> show text
