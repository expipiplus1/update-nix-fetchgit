{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Update.Nix.FetchGit.Utils
  ( RepoLocation(..)
  , extractUrlString
  , extractAttr
  , exprText
  , exprSpan
  , traverse2
  , sequenceA2
  , mapConcurrently2
  ) where

import           Control.Concurrent.Async    (mapConcurrently)
import           Data.Data                   (Data)
import           Data.Functor.Compose        (Compose (..))
import           Data.Maybe                  (catMaybes)
import           Data.Monoid                 ((<>))
import           Data.Text
import           Nix.Expr
import           Update.Nix.FetchGit.Warning
import           Update.Span

-- | A repo can either be on github, or have an url specified
data RepoLocation = GitHub{ owner :: Text
                          , repo  :: Text
                          }
                  | URL Text
  deriving (Show, Data)

-- | Get the url from either a nix expression for the url or a repo and owner
-- expression.
extractUrlString :: RepoLocation -> Text
extractUrlString = \case
  URL u -> u
  GitHub o r -> "git@github.com:" <> o <> "/" <> r <> ".git"

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

traverse2 :: (Traversable t, Traversable s, Applicative f)
          => (a -> f b) -> t (s a) -> f (t (s b))
traverse2 f = fmap getCompose . traverse f . Compose

sequenceA2 :: (Traversable t, Traversable s, Applicative f)
            => t (s (f a)) -> f (t (s a))
sequenceA2 = fmap getCompose . sequenceA . Compose

mapConcurrently2 :: (Traversable t, Traversable s) => (a -> IO b) -> t (s a) -> IO (t (s b))
mapConcurrently2 f = fmap getCompose . mapConcurrently f . Compose

