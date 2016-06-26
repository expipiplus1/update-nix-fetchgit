{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Update.Nix.FetchGit.Utils
  ( extractAttr
  , exprText
  , exprSpan
  ) where

import           Data.Maybe                  (catMaybes)
import           Data.Text
import           Nix.Expr
import           Update.Nix.FetchGit.Warning
import           Update.Span

-- | Get the string value of a particular expression, returns a 'Warning' if
-- the expression is not a string value.
--
-- TODO: Use 'evalExpr' here
exprText :: NExprLoc -> Either Warning Text
exprText = \case
  (AnnE _ (NStr (DoubleQuoted [Plain t]))) -> pure t
  e -> Left (NotAString e)

-- | Get the 'SourceSpan' covering a particular expression.
exprSpan :: Text -> NExprLoc -> Either Warning SourceSpan
exprSpan t (AnnE (SrcSpan b e) _) = SourceSpan <$> deltaToSourcePos t b
                                               <*> deltaToSourcePos t e

-- | Go from a 'Delta' to a 'SourcePos' in a particular bit of Text
deltaToSourcePos :: Text -> Delta -> Either Warning SourcePos
deltaToSourcePos t = \case
  Directed _ l c _ _ -> pure $ linearizeSourcePos t l c
  d -> Left (BadSourcePos d)

-- | Extract a named attribute from an attrset
extractAttr :: Text -> [Binding a] -> Either Warning a
extractAttr name bs = case catMaybes (matchAttr name <$> bs) of
  []  -> Left (MissingAttr name)
  [x] -> Right x
  _   -> Left (DuplicateAttrs name)

-- | Returns 'Just value' if this attribute's key matches the text, otherwise
-- Nothing.
matchAttr :: Text -> Binding a -> Maybe a
matchAttr t = \case
  NamedVar [StaticKey t'] x | t == t' -> Just x
  NamedVar _ _ -> Nothing
  Inherit _ _  -> Nothing
