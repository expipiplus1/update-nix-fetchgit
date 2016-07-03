{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Update.Nix.FetchGit.Utils
  ( RepoLocation(..)
  , extractUrlString
  , quoteString
  , extractAttr
  , findAttr
  , exprText
  , exprSpan
  ) where

import           Data.Maybe                  (catMaybes)
import           Data.Monoid                 ((<>))
import           Data.Text
import           Nix.Expr
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Warning
import           Update.Span

-- | Get the url from either a nix expression for the url or a repo and owner
-- expression.
extractUrlString :: RepoLocation -> Text
extractUrlString = \case
  URL u -> u
  GitHub o r -> "git@github.com:" <> o <> "/" <> r <> ".git"

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
exprSpan (AnnE (SrcSpan begin end) _) =
         SourceSpan (deltaToSourcePos begin) (deltaToSourcePos end)

-- | Go from a 'Delta' to a 'SourcePos' using an irrefutable pattern.
deltaToSourcePos :: Delta -> SourcePos
deltaToSourcePos delta = (SourcePos line column)
                 where (Directed _ line column _ _) = delta

-- | Extract a named attribute from an attrset.
extractAttr :: Text -> [Binding a] -> Either Warning a
extractAttr name bs = case catMaybes (matchAttr name <$> bs) of
  [x] -> Right x
  []  -> Left (MissingAttr name)
  _   -> Left (DuplicateAttrs name)

-- | Find a named attribute in an attrset.  This is appropriate for
-- the case when a missing attribute is not an error.
findAttr :: Text -> [Binding a] -> Either Warning (Maybe a)
findAttr name bs = case catMaybes (matchAttr name <$> bs) of
  [x] -> Right (Just x)
  []  -> Right Nothing
  _   -> Left (DuplicateAttrs name)

-- | Returns 'Just value' if this attribute's key matches the text, otherwise
-- Nothing.
matchAttr :: Text -> Binding a -> Maybe a
matchAttr t = \case
  NamedVar [StaticKey t'] x | t == t' -> Just x
  NamedVar _ _ -> Nothing
  Inherit _ _  -> Nothing
