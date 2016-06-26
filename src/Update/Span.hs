{-# LANGUAGE LambdaCase #-}

module Update.Span
  ( SpanUpdate(..)
  , SourcePos(..)
  , SourceSpan(..)
  , updateSpan
  , updateSpans
  , linearizeSourcePos
  ) where

import           Data.Int    (Int64)
import           Data.List   (genericTake, sortOn)
import           Data.Monoid ((<>))
import           Data.Text   (Text, length, lines, splitAt)
import           Prelude     hiding (length, lines, splitAt)

data SourcePos = SourcePos{ characterOffset :: !Int64
                          }
  deriving (Show, Eq, Ord)

data SourceSpan = SourceSpan{ spanBegin :: SourcePos
                            , spanEnd   :: SourcePos
                            }
  deriving (Show)

data SpanUpdate = SpanUpdate{ updateSourceSpan :: SourceSpan
                            , updateContents   :: Text
                            }
  deriving (Show)

updateSpans :: [SpanUpdate] -> Text -> Maybe Text
updateSpans us t =
  let sortedSpans = sortOn (spanBegin . updateSourceSpan) us
      anyOverlap = any (uncurry overlaps)
                       (zip <*> tail $ updateSourceSpan <$> sortedSpans)
      composedUpdates = composeUpdates sortedSpans
  in if anyOverlap
       then Nothing
       else Just (composedUpdates t)

overlaps :: SourceSpan -> SourceSpan -> Bool
overlaps (SourceSpan b1 e1) (SourceSpan b2 e2) =
  b2 >= b1 && b2 < e1 || e2 >= b1 && e2 < e1

-- | Compose some updates such that each one
composeUpdates :: [SpanUpdate] -> Text -> Text
composeUpdates us = fst $ foldl f (id, 0) us
  where f (updateFun, offsetAdjustment) (SpanUpdate s t) =
          let fixedUpdate = SpanUpdate (adjustSpan offsetAdjustment s) t
              replacementLength = fromIntegral (length t)
              offsetAdjustment' = offsetAdjustment + replacementLength - spanLength s
          in (updateSpan fixedUpdate . updateFun, offsetAdjustment')

adjustSpan :: Int64 -> SourceSpan -> SourceSpan
adjustSpan offset (SourceSpan b e) = SourceSpan (adjustPos b) (adjustPos e)
  where adjustPos (SourcePos i) = SourcePos (i + offset)

spanLength :: SourceSpan -> Int64
spanLength (SourceSpan (SourcePos b) (SourcePos e)) = e - b

updateSpan :: SpanUpdate -> Text -> Text
updateSpan (SpanUpdate (SourceSpan b e) r) t =
  let (before, _) = split b t
      (_, end) = split e t
  in before <> r <> end

split :: SourcePos -> Text -> (Text, Text)
split (SourcePos c) = splitAt (fromIntegral c)

-- | This probably fails on crazy texts with multi character line breaks.
linearizeSourcePos :: Text -- ^ The string to linearize in
                   -> Int64 -- ^ The line offset
                   -> Int64 -- ^ The column offset
                   -> SourcePos
linearizeSourcePos t l c = SourcePos charOffset
  where charOffset = fromIntegral lineCharOffset + c
        lineCharOffset = sum . fmap ((+1) . length) . genericTake l . lines $ t

