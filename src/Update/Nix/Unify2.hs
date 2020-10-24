{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}

module Update.Nix.Unify2
  ( match
  , WithHoles(..)
  ) where

import           Data.Fix                       ( Fix(Fix) )
import           Data.Foldable
import           Data.List                      ( sortOn )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Monoid
import           Data.Text                      ( Text )
import           GHC.Generics
import           Nix

-- | Match a tree with holes against a tree without holes, returning the values
-- of the holes if it matches.
match :: Matchable t => WithHoles t v -> Fix t -> Maybe [(v, Fix t)]
match = fmap (`appEndo` []) .: go
 where
  go = \case
    Hole v -> \t -> Just (Endo ((v, t) :))
    Term s -> \(Fix t) -> do
      m <- zipMatchLeft s t
      fmap fold . traverse (uncurry go) . toList $ m

-- | Like 'Fix' but each layer could instead be a 'Hole'
data WithHoles t v
  = Hole !v
  | Term !(t (WithHoles t v))

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

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
    in  to1 <$> gZipMatch (from1 (NSet t1 bs1')) (from1 (NSet t2 bs2'))

  zipMatchLeft (NLet bs1 e1) (NLet bs2 e2) =
    let (bs1', bs2') = reduceBindings bs1 bs2
    in  to1 <$> gZipMatch (from1 (NLet bs1' e1)) (from1 (NLet bs2' e2))

  zipMatchLeft (NAbs (Param "_") e1) (NAbs _ e2) = do
    pure $ NAbs (Param "_") (e1, e2)

  zipMatchLeft l r = to1 <$> gZipMatch (from1 l) (from1 r)

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
-- helpers
----------------------------------------------------------------

-- | Match a composition of 'Matchable' things
zipMatchLeft2
  :: (Matchable f, Matchable t) => t (f a) -> t (f b) -> Maybe (t (f (a, b)))
zipMatchLeft2 a b = zipMatchLeft a b >>= traverse (uncurry zipMatchLeft)

----------------------------------------------------------------
-- Matchable
----------------------------------------------------------------

class Traversable t => Matchable t where
  -- | Match one level of structure, returning the matched structure with sub
  -- structures to match. Needle is the first argument, matchee is the second.
  zipMatchLeft :: t a -> t b -> Maybe (t (a,b))
  default zipMatchLeft
    :: (Generic1 t, GMatchable (Rep1 t))
    => t a
    -> t b
    -> Maybe (t (a, b))
  zipMatchLeft l r = to1 <$> gZipMatch (from1 l) (from1 r)

----------------------------------------------------------------
-- Generic Instance for Matchable
----------------------------------------------------------------

class (Traversable t, Generic1 t) => GMatchable t where
  gZipMatch :: t a -> t b -> Maybe (t (a,b))

instance GMatchable t => GMatchable (M1 m i t) where
  gZipMatch (M1 l) (M1 r) = M1 <$> gZipMatch l r

instance GMatchable U1 where
  gZipMatch _ _ = Just U1

instance Eq c => GMatchable (K1 m c) where
  gZipMatch (K1 l) (K1 r) | l == r    = Just (K1 l)
                          | otherwise = Nothing

instance GMatchable Par1 where
  gZipMatch (Par1 l) (Par1 r) = Just . Par1 $ (l, r)

instance Matchable x => GMatchable (Rec1 x) where
  gZipMatch (Rec1 l) (Rec1 r) = Rec1 <$> zipMatchLeft l r

instance (GMatchable l, GMatchable r) => GMatchable (l :+: r) where
  gZipMatch (L1 l) (L1 r) = L1 <$> gZipMatch l r
  gZipMatch (R1 l) (R1 r) = R1 <$> gZipMatch l r
  gZipMatch _      _      = Nothing

instance (GMatchable l, GMatchable r) => GMatchable (l :*: r) where
  gZipMatch (l1 :*: r1) (l2 :*: r2) =
    (:*:) <$> gZipMatch l1 l2 <*> gZipMatch r1 r2

instance (Matchable a, GMatchable b) => GMatchable (a :.: b) where
  gZipMatch (Comp1 l) (Comp1 r) = do
    x <- zipMatchLeft l r >>= traverse
      (\case
        (a1, a2) -> gZipMatch a1 a2
      )
    pure (Comp1 x)
