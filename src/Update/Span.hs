{-# LANGUAGE DeriveDataTypeable #-}

-- | This module deals with updating spans of characters in values of type Text.
--
-- It defines some helper types and functions to apply these "updates".

module Update.Span
  ( SpanUpdate(..)
  , SourcePos(..)
  , SourceSpan(..)
  , updateSpan
  , updateSpans
  , linearizeSourcePos
  ) where

import           Control.Exception (assert)
import           Data.Data   (Data)
import           Data.Int    (Int64)
import           Data.List   (genericTake, sortOn)
import           Data.Monoid ((<>))
import           Data.Text   (Text, length, lines, splitAt)
import           Prelude     hiding (length, lines, splitAt)

-- | A position in a text file
data SourcePos = SourcePos{ sourcePosRow :: Int64,
                            sourcePosColumn :: Int64
                          }
  deriving (Show, Eq, Ord, Data)

-- | A span of characters with a beginning and an end.
--
-- 'spanEnd' must be greater than 'spanBegin'
data SourceSpan = SourceSpan{ sourceSpanBegin :: SourcePos
                            , sourceSpanEnd   :: SourcePos
                            }
  deriving (Show, Data)

-- | A span and some text to replace it with.
-- They don't have to be the same length.
data SpanUpdate = SpanUpdate{ spanUpdateSpan     :: SourceSpan
                            , spanUpdateContents :: Text
                            }
  deriving (Show, Data)

-- | Update many spans in a file. They must be non-overlapping.
updateSpans :: [SpanUpdate] -> Text -> Text
updateSpans us t =
  let sortedSpans = sortOn (sourceSpanBegin . spanUpdateSpan) us
      anyOverlap = any (uncurry overlaps)
                       (zip <*> tail $ spanUpdateSpan <$> sortedSpans)
  in
    assert (not anyOverlap)
    (foldr updateSpan t sortedSpans)

-- | Update a single span of characters inside a text value. If you're updating
-- multiples spans it's best to use 'updateSpans'.
updateSpan :: SpanUpdate -> Text -> Text
updateSpan (SpanUpdate (SourceSpan b e) r) t =
  let (before, _) = split b t
      (_, end) = split e t
  in before <> r <> end

-- | Do two spans overlap
overlaps :: SourceSpan -> SourceSpan -> Bool
overlaps (SourceSpan b1 e1) (SourceSpan b2 e2) =
  b2 >= b1 && b2 < e1 || e2 >= b1 && e2 < e1

-- | Split some text at a particular 'SourcePos'
split :: SourcePos -> Text -> (Text, Text)
split (SourcePos row col) t =
  splitAt (fromIntegral (linearizeSourcePos t row col)) t

-- | Go from a line and column representation to a single character offset from
-- the beginning of the text.
--
-- This probably fails on crazy texts with multi character line breaks.
linearizeSourcePos :: Text -- ^ The string to linearize in
                   -> Int64 -- ^ The line offset
                   -> Int64 -- ^ The column offset
                   -> Int64 -- ^ The character offset
linearizeSourcePos t l c = fromIntegral lineCharOffset + c
   where lineCharOffset = sum . fmap ((+1) . length) . genericTake l . lines $ t
