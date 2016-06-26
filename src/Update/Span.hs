{-# LANGUAGE LambdaCase #-}

module Update.Span
  ( SpanUpdate(..)
  , SourcePos(..)
  , SourceSpan(..)
  , updateSpan
  , linearizeSourcePos
  ) where

import           Data.Int    (Int64)
import           Data.List   (genericTake)
import           Data.Monoid ((<>))
import           Data.Text   (Text, length, lines, splitAt)
import           Prelude     hiding (length, lines, splitAt)

data SourcePos = SourcePos{ characterOffset :: !Int64
                          }
  deriving (Show)

data SourceSpan = SourceSpan{ spanBegin :: SourcePos
                            , spanEnd   :: SourcePos
                            }
  deriving (Show)

data SpanUpdate = SpanUpdate{ updateSourceSpan :: SourceSpan
                            , updateContents   :: Text
                            }
  deriving (Show)

updateSpan :: SpanUpdate -> Text -> Text
updateSpan (SpanUpdate (SourceSpan b e) r) t =
  let (before, _) = split b t
      (_, end) = split e t
  in before <> r <> end

split :: SourcePos -> Text -> (Text, Text)
split (SourcePos c) = splitAt (fromIntegral c)

linearizeSourcePos :: Text -- ^ The string to linearize in
                   -> Int64 -- ^ The line offset
                   -> Int64 -- ^ The column offset
                   -> SourcePos
linearizeSourcePos t l c = SourcePos charOffset
  where charOffset = fromIntegral lineCharOffset + c
        lineCharOffset = sum . fmap ((+1) . length) . genericTake l . lines $ t

