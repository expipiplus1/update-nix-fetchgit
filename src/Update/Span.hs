
-- | This module deals with updating spans of characters in values of type Text.
--
-- It defines some helper types and functions to apply these "updates".

module Update.Span
  ( SpanUpdate(..)
  , SrcSpan(..)
  , SourcePos(..)
  , updateSpan
  , updateSpans
  , linearizeSourcePos
  , prettyPrintSourcePos
  , split
  ) where

import           Control.Exception              ( assert )
import           Data.Data                      ( Data )
import           Data.Int                       ( Int64 )
import           Data.List                      ( genericTake
                                                , sortOn
                                                )
import           Data.Text                      ( Text
                                                , length
                                                , lines
                                                , splitAt
                                                )
import           Nix.Expr.Types.Annotated
import           Prelude                 hiding ( length
                                                , lines
                                                , splitAt
                                                )

-- | A span and some text to replace it with.
-- They don't have to be the same length.
data SpanUpdate = SpanUpdate
  { spanUpdateSpan     :: SrcSpan
  , spanUpdateContents :: Text
  }
  deriving (Show, Data)

-- | Update many spans in a file. They must be non-overlapping.
updateSpans :: [SpanUpdate] -> Text -> Text
updateSpans us t =
  let sortedSpans = sortOn (spanBegin . spanUpdateSpan) us
      anyOverlap =
        any (uncurry overlaps) (zip <*> tail $ spanUpdateSpan <$> sortedSpans)
  in  assert (not anyOverlap) (foldr updateSpan t sortedSpans)

-- | Update a single span of characters inside a text value. If you're updating
-- multiples spans it's best to use 'updateSpans'.
updateSpan :: SpanUpdate -> Text -> Text
updateSpan (SpanUpdate (SrcSpan b e) r) t =
  let (before, _  ) = split b t
      (_     , end) = split e t
  in  before <> r <> end

-- | Do two spans overlap
overlaps :: SrcSpan -> SrcSpan -> Bool
overlaps (SrcSpan b1 e1) (SrcSpan b2 e2) =
  b2 >= b1 && b2 < e1 || e2 >= b1 && e2 < e1

-- | Split some text at a particular 'SourcePos'
split :: SourcePos -> Text -> (Text, Text)
split (SourcePos _ row col) t = splitAt
  (fromIntegral
    (linearizeSourcePos t
                        (fromIntegral (unPos row - 1))
                        (fromIntegral (unPos col - 1))
    )
  )
  t

-- | Go from a line and column representation to a single character offset from
-- the beginning of the text.
--
-- This probably fails on crazy texts with multi character line breaks.
linearizeSourcePos
  :: Text -- ^ The string to linearize in
  -> Int64 -- ^ The line offset
  -> Int64 -- ^ The column offset
  -> Int64 -- ^ The character offset
linearizeSourcePos t l c = fromIntegral lineCharOffset + c
 where
  lineCharOffset = sum . fmap ((+ 1) . length) . genericTake l . lines $ t

prettyPrintSourcePos :: SourcePos -> String
prettyPrintSourcePos (SourcePos _ row column) =
  show (unPos row) <> ":" <> show (unPos column)
