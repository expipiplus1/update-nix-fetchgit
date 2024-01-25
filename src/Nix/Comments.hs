module Nix.Comments
  ( annotateWithComments,
    Comment,
    NExprCommentsF,
    NExprComments,
  )
where

import Data.Char (isSpace)
import Data.Fix
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector
  ( Vector,
    (!?),
  )
import Nix.Expr

type Comment = Text

type NExprCommentsF = AnnF (Maybe Comment) NExprLocF

type NExprComments = Fix NExprCommentsF

-- | A comment will be added to an expression if it occurs immediately after
-- the expression in the source, i.e. on the same line with only space and ';'
-- in between.
--
-- >>> import Nix.Parser
-- >>> import Nix.Pretty
-- >>> import Data.Vector
-- >>> import Data.Foldable
-- >>> lines = T.pack <$> ["1 # foo", "+ {a=2; # bar","} # baz"]
-- >>> str = T.unlines $ lines
-- >>> Success nix = parseNixTextLoc str
-- >>> ann = annotateWithComments (fromList lines) nix
-- >>> fixUniverse e = e : (fixUniverse =<< Data.Foldable.toList (unFix e))
-- >>> pretty e@(Fix (Compose (Ann comment _)))= (prettyNix (stripAnnotation (stripAnnotation e)), comment)
-- >>> pretty <$> fixUniverse ann
-- [(1 + { a = 2; },Just "baz"),(1,Just "foo"),({ a = 2; },Just "baz"),(2,Just "bar")]
annotateWithComments :: Vector Text -> NExprLoc -> NExprComments
annotateWithComments sourceLines = go
  where
    go :: NExprLoc -> NExprComments
    go = Fix . go' . fmap go . unFix

    go' :: NExprLocF f -> NExprCommentsF f
    go' e =
      let comment = case getSpanEnd . annotation . getCompose $ e of
            NSourcePos _ (NPos line) (NPos col) -> do
              theLine <- sourceLines !? (unPos line - 1)
              theLineAfterExpression <- dropMaybe (unPos col - 1) theLine
              let theLineAfterCruft =
                    T.dropWhile
                      (\c -> isSpace c || (c == ';'))
                      theLineAfterExpression
              ('#', theComment) <- T.uncons theLineAfterCruft
              pure (T.strip theComment)
       in Compose (AnnUnit comment e)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

dropMaybe :: Int -> Text -> Maybe Text
dropMaybe i t = if T.length t >= i then Just $ T.drop i t else Nothing
