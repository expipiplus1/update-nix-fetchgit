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

-- TODO: Use 'evalExpr' here
exprText :: NExprLoc -> Either Warning Text
exprText = \case
  (AnnE _ (NStr (DoubleQuoted [Plain t]))) -> pure t
  e -> Left (NotAString e)

exprSpan :: Text -> NExprLoc -> Either Warning SourceSpan
exprSpan t (AnnE (SrcSpan b e) _) = SourceSpan <$> deltaToSourcePos t b
                                               <*> deltaToSourcePos t e

deltaToSourcePos :: Text -> Delta -> Either Warning SourcePos
deltaToSourcePos t = \case
  Directed _ l c _ _ -> pure $ linearizeSourcePos t l c
  d -> Left (BadSourcePos d)

extractAttr :: Text -> [Binding a] -> Either Warning a
extractAttr name bs = case catMaybes (matchAttr name <$> bs) of
  []  -> Left (MissingAttr name)
  [x] -> Right x
  _   -> Left (DuplicateAttrs name)

matchAttr :: Text -> Binding a -> Maybe a
matchAttr t = \case
  NamedVar [StaticKey t'] x | t == t' -> Just x
  NamedVar _ _ -> Nothing
  Inherit _ _  -> Nothing
