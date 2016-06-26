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

data SourcePos = SourcePos{ line   :: !Int64
                          , column :: !Int64
                          }
  deriving (Show)

data SourcePosLinear = SourcePosLinear{ characterOffset :: !Int64
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
  let bLinear = linearizeSourcePos t b
      eLinear = linearizeSourcePos t e
      (before, _) = split bLinear t
      (_, end) = split eLinear t
  in before <> r <> end

split :: SourcePosLinear -> Text -> (Text, Text)
split (SourcePosLinear c) = splitAt (fromIntegral c)

linearizeSourcePos :: Text -- ^ The string to linearize in
                   -> SourcePos
                   -> SourcePosLinear
linearizeSourcePos t (SourcePos l c) = SourcePosLinear charOffset
  where charOffset = fromIntegral lineCharOffset + c
        lineCharOffset = sum . fmap ((+1) . length) . genericTake l . lines $ t

