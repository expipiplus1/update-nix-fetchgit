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

exprSpan :: NExprLoc -> Either Warning SourceSpan
exprSpan (AnnE (SrcSpan b e) _) = SourceSpan <$> deltaToSourcePos b
                                             <*> deltaToSourcePos e

deltaToSourcePos :: Delta -> Either Warning SourcePos
deltaToSourcePos = \case
  Directed _ l c _ _ -> pure $ SourcePos l c
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
