{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}

module Update.Nix.Unify2 where

import           Data.Fix                       ( Fix(Fix) )
import           Data.Foldable
import           Data.List                      ( sortOn )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Text                      ( Text )
import           GHC.Generics
import           Nix
import Data.Monoid

data UTerm t v
  = UVar !v
  | UTerm !(t (UTerm t v))

unify :: Unifiable t => UTerm t v -> Fix t -> Maybe [(v, Fix t)]
unify = fmap (`appEndo` []) .: go
 where
  go = \case
    UVar  v -> \t -> Just (Endo ((v, t) :))
    UTerm s -> \(Fix t) -> do
      m <- zipMatchLeft s t
      fmap fold . traverse (uncurry go) . toList $ m

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)

----------------------------------------------------------------
-- Unifiable instance for NExprF and NExprLocF
----------------------------------------------------------------

-- | For attribute sets, all the needle's keys must be present, but keys in the
-- haystack can be missing; the same goes for let bindings.
--
-- If a function in the needle has @_@ as parameter, it matches everything.
instance Unifiable NExprF where

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
reduceBindings needle haystack =
  let sortBindings :: [Binding r] -> [Binding r]
      sortBindings = sortOn (() <$)
      filterBindings :: [Binding q] -> [Binding r] -> [Binding r]
      filterBindings as bs =
        let simplifyName :: NAttrPath a -> NAttrPath ()
            simplifyName = fmap (() <$)
            names        = [ simplifyName p | NamedVar p _ _ <- as ]
        in  [ b | b@(NamedVar p _ _) <- bs, simplifyName p `elem` names ]
  in  (sortBindings needle, sortBindings (filterBindings needle haystack))

--
-- hnix types
--

instance Unifiable NString where

instance Unifiable (Antiquoted Text) where

-- | The matched pair uses the source location of the first argument
instance Unifiable Binding where
  zipMatchLeft (NamedVar p1 v1 _) (NamedVar p2 v2 l) = do
    p <- zipMatchLeft2 p1 p2
    pure (NamedVar p (v1, v2) l)

  zipMatchLeft (Inherit x1 ys1 l) (Inherit x2 ys2 _) = do
    x  <- zipMatchLeft x1 x2
    ys <- zipMatchLeft2 ys1 ys2
    pure (Inherit x ys l)

  zipMatchLeft _ _ = Nothing

-- | No Generic1 instance
instance Unifiable NKeyName where
  zipMatchLeft (StaticKey k1) (StaticKey k2) | k1 == k2 = Just (StaticKey k1)
  zipMatchLeft (DynamicKey EscapedNewline) (DynamicKey EscapedNewline) =
    Just (DynamicKey EscapedNewline)
  zipMatchLeft (DynamicKey (Plain k1)) (DynamicKey (Plain k2)) = do
    k <- zipMatchLeft k1 k2
    pure $ DynamicKey (Plain k)
  zipMatchLeft (DynamicKey (Antiquoted k1)) (DynamicKey (Antiquoted k2)) =
    pure $ DynamicKey (Antiquoted (k1, k2))
  zipMatchLeft _ _ = Nothing

instance Unifiable Params where

-- | Doesn't require the annotations to match, returns the second annotation.
instance Unifiable (Ann ann) where
  zipMatchLeft (Ann _ a1) (Ann ann2 a2) =
    Just $ Ann ann2 (a1, a2)

--
-- base types
--

instance Unifiable [] where

instance Unifiable NonEmpty where

instance Unifiable Maybe where

instance Eq a => Unifiable ((,) a) where

instance (Unifiable f, Unifiable g)=> Unifiable (Compose f g) where

----------------------------------------------------------------
-- helpers
----------------------------------------------------------------

-- | Match a composition of 'Unifiable' things
zipMatchLeft2
  :: (Unifiable f, Unifiable t) => t (f a) -> t (f b) -> Maybe (t (f (a, b)))
zipMatchLeft2 a b = zipMatchLeft a b >>= traverse (uncurry zipMatchLeft)

----------------------------------------------------------------
-- Unifiable
----------------------------------------------------------------

class Traversable t => Unifiable t where
  -- | Match one level of structure, returning the matched structure with sub
  -- structures to match. Needle is the first argument, haystack is the second.
  zipMatchLeft :: t a -> t b -> Maybe (t (a,b))
  default zipMatchLeft
    :: (Generic1 t, GUnifiable (Rep1 t))
    => t a
    -> t b
    -> Maybe (t (a, b))
  zipMatchLeft l r = to1 <$> gZipMatch (from1 l) (from1 r)

----------------------------------------------------------------
-- Generic Instance for Unifiable
----------------------------------------------------------------

class (Traversable t, Generic1 t) => GUnifiable t where
  gZipMatch :: t a -> t b -> Maybe (t (a,b))

instance GUnifiable t => GUnifiable (M1 m i t) where
  gZipMatch (M1 l) (M1 r) = M1 <$> gZipMatch l r

instance GUnifiable U1 where
  gZipMatch _ _ = Just U1

instance Eq c => GUnifiable (K1 m c) where
  gZipMatch (K1 l) (K1 r) | l == r    = Just (K1 l)
                          | otherwise = Nothing

instance GUnifiable Par1 where
  gZipMatch (Par1 l) (Par1 r) = Just . Par1 $ (l, r)

instance Unifiable x => GUnifiable (Rec1 x) where
  gZipMatch (Rec1 l) (Rec1 r) = Rec1 <$> zipMatchLeft l r

instance (GUnifiable l, GUnifiable r) => GUnifiable (l :+: r) where
  gZipMatch (L1 l) (L1 r) = L1 <$> gZipMatch l r
  gZipMatch (R1 l) (R1 r) = R1 <$> gZipMatch l r
  gZipMatch _      _      = Nothing

instance (GUnifiable l, GUnifiable r) => GUnifiable (l :*: r) where
  gZipMatch (l1 :*: r1) (l2 :*: r2) =
    (:*:) <$> gZipMatch l1 l2 <*> gZipMatch r1 r2

instance (Unifiable a, GUnifiable b) => GUnifiable (a :.: b) where
  gZipMatch (Comp1 l) (Comp1 r) = do
    x <- zipMatchLeft l r >>= traverse
      (\case
        (a1, a2) -> gZipMatch a1 a2
      )
    pure (Comp1 x)
