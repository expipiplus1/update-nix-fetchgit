{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}

module Nix.Match
  ( match
  , findMatches
  , Matchable(..)
  , GMatchable(..)
  , WithHoles(..)
  , addHoles
  , addHolesLoc
  ) where

import           Data.Fix
import           Data.Foldable
import           Data.List                      ( sortOn )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Monoid
import           Data.Text                      ( Text )
import           GHC.Generics
import           Nix
import           Control.Category               ( (>>>) )

-- | Like 'Fix' but each layer could instead be a 'Hole'
data WithHoles t v
  = Hole !v
  | Term !(t (WithHoles t v))

-- | Match a tree with holes against a tree without holes, returning the values
-- of the holes if it matches.
--
-- 'NExprF' and 'NExprLocF' are both instances of 'Matchable'
--
-- >>> import Nix.TH
-- >>> match (addHoles [nix|{foo = x: ^foo; bar = ^bar;}|]) [nix|{foo = x: "hello"; bar = "world"; baz = "!";}|]
-- Just [("bar",Fix (NStr (DoubleQuoted [Plain "world"]))),("foo",Fix (NStr (DoubleQuoted [Plain "hello"])))]
match :: Matchable t => WithHoles t v -> Fix t -> Maybe [(v, Fix t)]
match = fmap (`appEndo` []) .: go
 where
  go = \case
    Hole v -> \t -> Just (Endo ((v, t) :))
    Term s -> \(Fix t) -> do
      m <- zipMatchLeft s t
      fmap fold . traverse (uncurry go) . toList $ m

-- | Find all the needles in a haystack, returning the matched expression as
-- well as their filled holes. Results are returned productively in preorder.
findMatches
  :: Matchable t
  => WithHoles t v
  -- ^ Needle
  -> Fix t
  -- ^ Haystack
  -> [(Fix t, [(v, Fix t)])]
findMatches needle haystack =
  [ (s, r) | s <- fixUniverse haystack, Just r <- pure $ match needle s ]

-- | Get every @f@ in a @Fix f@ in preorder.
fixUniverse :: Foldable f => Fix f -> [Fix f]
fixUniverse e = e : (fixUniverse =<< toList (unFix e))

-- | Make syntactic holes into 'Hole's
addHoles :: NExpr -> WithHoles NExprF Text
addHoles = unFix >>> \case
  NSynHole n -> Hole n
  e          -> Term . fmap addHoles $ e

-- | Make syntactic holes into 'Hole's
addHolesLoc :: NExprLoc -> WithHoles NExprLocF Text
addHolesLoc = unFix >>> \case
  Compose (Ann _ (NSynHole n)) -> Hole n
  e                            -> Term . fmap addHolesLoc $ e

----------------------------------------------------------------
-- Matchable
----------------------------------------------------------------

class Traversable t => Matchable t where
  -- | Match one level of structure, returning the matched structure with sub
  -- structures to match. Needle is the first argument, matchee is the second.
  --
  -- Unlike the @Unifiable@ class in the "unification-fd" package, this doesn't
  -- have to be a commutative operation, the needle will always be the first
  -- parameter and instances are free to treat if differently if appropriate.
  zipMatchLeft :: t a -> t b -> Maybe (t (a,b))
  default zipMatchLeft
    :: (Generic1 t, GMatchable (Rep1 t))
    => t a
    -> t b
    -> Maybe (t (a, b))
  zipMatchLeft l r = to1 <$> gZipMatchLeft (from1 l) (from1 r)

-- | Match a composition of 'Matchable' things
zipMatchLeft2
  :: (Matchable f, Matchable t) => t (f a) -> t (f b) -> Maybe (t (f (a, b)))
zipMatchLeft2 a b = zipMatchLeft a b >>= traverse (uncurry zipMatchLeft)

----------------------------------------------------------------
-- Matchable instance for NExprF and NExprLocF
----------------------------------------------------------------

-- | There are a few special cases when matching expressions to make writing
-- matchers nicer:
--
-- - For attrsets and let bindings, the matching is done on the needle's keys
--   only. i.e. the matchee may have extra keys which are ignored.
--
-- - If a function in the needle has @_@ as its parameter, it matches
--   everything, so @_@ acts as a wildcard pattern.
instance Matchable NExprF where

  zipMatchLeft (NSet t1 bs1) (NSet t2 bs2) =
    let (bs1', bs2') = reduceBindings bs1 bs2
    in  to1 <$> gZipMatchLeft (from1 (NSet t1 bs1')) (from1 (NSet t2 bs2'))

  zipMatchLeft (NLet bs1 e1) (NLet bs2 e2) =
    let (bs1', bs2') = reduceBindings bs1 bs2
    in  to1 <$> gZipMatchLeft (from1 (NLet bs1' e1)) (from1 (NLet bs2' e2))

  zipMatchLeft (NAbs (Param "_") e1) (NAbs _ e2) = do
    pure $ NAbs (Param "_") (e1, e2)
  zipMatchLeft l r = to1 <$> gZipMatchLeft (from1 l) (from1 r)

-- Don't filter bindings in the needle, as they must all be present
reduceBindings :: [Binding q] -> [Binding r] -> ([Binding q], [Binding r])
reduceBindings needle matchee =
  let sortBindings :: [Binding r] -> [Binding r]
      sortBindings = sortOn (() <$)
      filterBindings :: [Binding q] -> [Binding r] -> [Binding r]
      filterBindings as bs =
        let simplifyName :: NAttrPath a -> NAttrPath ()
            simplifyName = fmap (() <$)
            names        = [ simplifyName p | NamedVar p _ _ <- as ]
        in  [ b | b@(NamedVar p _ _) <- bs, simplifyName p `elem` names ]
  in  (sortBindings needle, sortBindings (filterBindings needle matchee))

--
-- hnix types
--

instance Matchable NString where

instance Matchable (Antiquoted Text) where

-- | The matched pair uses the source location of the first argument
instance Matchable Binding where
  zipMatchLeft (NamedVar p1 v1 _) (NamedVar p2 v2 l) = do
    p <- zipMatchLeft2 p1 p2
    pure (NamedVar p (v1, v2) l)

  zipMatchLeft (Inherit x1 ys1 l) (Inherit x2 ys2 _) = do
    x  <- zipMatchLeft x1 x2
    ys <- zipMatchLeft2 ys1 ys2
    pure (Inherit x ys l)

  zipMatchLeft _ _ = Nothing

-- | No Generic1 instance
instance Matchable NKeyName where
  zipMatchLeft (StaticKey k1) (StaticKey k2) | k1 == k2 = Just (StaticKey k1)
  zipMatchLeft (DynamicKey EscapedNewline) (DynamicKey EscapedNewline) =
    Just (DynamicKey EscapedNewline)
  zipMatchLeft (DynamicKey (Plain k1)) (DynamicKey (Plain k2)) = do
    k <- zipMatchLeft k1 k2
    pure $ DynamicKey (Plain k)
  zipMatchLeft (DynamicKey (Antiquoted k1)) (DynamicKey (Antiquoted k2)) =
    pure $ DynamicKey (Antiquoted (k1, k2))
  zipMatchLeft _ _ = Nothing

instance Matchable Params where

-- | Doesn't require the annotations to match, returns the second annotation.
instance Matchable (Ann ann) where
  zipMatchLeft (Ann _ a1) (Ann ann2 a2) = Just $ Ann ann2 (a1, a2)

--
-- base types
--

instance Matchable [] where

instance Matchable NonEmpty where

instance Matchable Maybe where

instance Eq a => Matchable ((,) a) where

instance (Matchable f, Matchable g)=> Matchable (Compose f g) where


----------------------------------------------------------------
-- Generic Instance for Matchable
----------------------------------------------------------------

-- | A class used in the @default@ definition for 'zipMatchLeft'
class (Traversable t, Generic1 t) => GMatchable t where
  gZipMatchLeft :: t a -> t b -> Maybe (t (a,b))

instance GMatchable t => GMatchable (M1 m i t) where
  gZipMatchLeft (M1 l) (M1 r) = M1 <$> gZipMatchLeft l r

instance GMatchable U1 where
  gZipMatchLeft _ _ = Just U1

instance Eq c => GMatchable (K1 m c) where
  gZipMatchLeft (K1 l) (K1 r) | l == r    = Just (K1 l)
                              | otherwise = Nothing

instance GMatchable Par1 where
  gZipMatchLeft (Par1 l) (Par1 r) = Just . Par1 $ (l, r)

instance Matchable x => GMatchable (Rec1 x) where
  gZipMatchLeft (Rec1 l) (Rec1 r) = Rec1 <$> zipMatchLeft l r

instance (GMatchable l, GMatchable r) => GMatchable (l :+: r) where
  gZipMatchLeft (L1 l) (L1 r) = L1 <$> gZipMatchLeft l r
  gZipMatchLeft (R1 l) (R1 r) = R1 <$> gZipMatchLeft l r
  gZipMatchLeft _      _      = Nothing

instance (GMatchable l, GMatchable r) => GMatchable (l :*: r) where
  gZipMatchLeft (l1 :*: r1) (l2 :*: r2) =
    (:*:) <$> gZipMatchLeft l1 l2 <*> gZipMatchLeft r1 r2

instance (Matchable a, GMatchable b) => GMatchable (a :.: b) where
  gZipMatchLeft (Comp1 l) (Comp1 r) = do
    x <- zipMatchLeft l r >>= traverse (uncurry gZipMatchLeft)
    pure (Comp1 x)


----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)
