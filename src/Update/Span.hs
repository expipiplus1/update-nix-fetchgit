{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}

-- | This module deals with updating spans characters in values of type Text.
--
-- It defines some helper types and function to apply these "updates".

module Update.Span
  ( SpanUpdate(..)
  , SourcePos(..)
  , SourceSpan(..)
  , updateSpan
  , updateSpans
  , linearizeSourcePos
  ) where

import           Data.Data   (Data)
import           Data.Int    (Int64)
import           Data.List   (genericTake, sortOn)
import           Data.Monoid ((<>))
import           Data.Text   (Text, length, lines, splitAt)
import           Prelude     hiding (length, lines, splitAt)

-- | A position in a text file
newtype SourcePos = SourcePos{ characterOffset :: Int64
                             }
  deriving (Show, Eq, Ord, Data)

-- | A span of characters with a beginning and an end.
--
-- 'spanEnd' must be greater than 'spanBegin'
data SourceSpan = SourceSpan{ spanBegin :: SourcePos
                            , spanEnd   :: SourcePos
                            }
  deriving (Show, Data)

-- | A span, and some text to insert there instead, they don't have to be the
-- same length.
data SpanUpdate = SpanUpdate{ updateSourceSpan :: SourceSpan
                            , updateContents   :: Text
                            }
  deriving (Show, Data)

-- | Update many spans in a file. This function returns 'Nothing' if the spans
-- overlap. It modifies the offsets of the spans if earlier spans insert text
-- of a different length to what they're replacing.
updateSpans :: [SpanUpdate] -> Text -> Maybe Text
updateSpans us t =
  let sortedSpans = sortOn (spanBegin . updateSourceSpan) us
      anyOverlap = any (uncurry overlaps)
                       (zip <*> tail $ updateSourceSpan <$> sortedSpans)
      composedUpdates = composeUpdates sortedSpans
  in if anyOverlap
       then Nothing
       else Just (composedUpdates t)

-- | Update a single span of characters inside a text value. If you're updating
-- multiples spans it's best to use 'updateSpans'
updateSpan :: SpanUpdate -> Text -> Text
updateSpan (SpanUpdate (SourceSpan b e) r) t =
  let (before, _) = split b t
      (_, end) = split e t
  in before <> r <> end

-- | Do two spans overlap
overlaps :: SourceSpan -> SourceSpan -> Bool
overlaps (SourceSpan b1 e1) (SourceSpan b2 e2) =
  b2 >= b1 && b2 < e1 || e2 >= b1 && e2 < e1

-- | Compose some updates such that each one's location is adjusted according
-- to more or fewer characters created by earlier spans.
composeUpdates :: [SpanUpdate] -> Text -> Text
composeUpdates us = fst $ foldl f (id, 0) us
  where f (updateFun, offsetAdjustment) (SpanUpdate s t) =
          let fixedUpdate = SpanUpdate (adjustSpan offsetAdjustment s) t
              replacementLength = fromIntegral (length t)
              offsetAdjustment' = offsetAdjustment + replacementLength - spanLength s
          in (updateSpan fixedUpdate . updateFun, offsetAdjustment')

-- | adjust the location of a span according to some offset.
adjustSpan :: Int64 -> SourceSpan -> SourceSpan
adjustSpan offset (SourceSpan b e) = SourceSpan (adjustPos b) (adjustPos e)
  where adjustPos (SourcePos i) = SourcePos (i + offset)

-- | How long is the text a span represents
spanLength :: SourceSpan -> Int64
spanLength (SourceSpan (SourcePos b) (SourcePos e)) = e - b

-- | Split some text at a particular 'SourcePos'
split :: SourcePos -> Text -> (Text, Text)
split (SourcePos c) = splitAt (fromIntegral c)

-- | Go from a line and column representation to a single character offset from
-- the beginning of the text.
--
-- This probably fails on crazy texts with multi character line breaks.
linearizeSourcePos :: Text -- ^ The string to linearize in
                   -> Int64 -- ^ The line offset
                   -> Int64 -- ^ The column offset
                   -> SourcePos
linearizeSourcePos t l c = SourcePos charOffset
  where charOffset = fromIntegral lineCharOffset + c
        lineCharOffset = sum . fmap ((+1) . length) . genericTake l . lines $ t

