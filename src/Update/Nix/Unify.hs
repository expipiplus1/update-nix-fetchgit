{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Update.Nix.Unify
  () where

import           Control.Unification            ( Unifiable(..) )
import           Data.List                      ( sortOn )
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Text                      ( Text )
import           GHC.Generics
import           Nix

----------------------------------------------------------------
-- Unifiable instance for NExprF and NExprLocF
----------------------------------------------------------------

-- | For attribute sets, these are compared only on the intersection of keys
instance Unifiable NExprF where
  zipMatch (NSet t1 bs1) (NSet t2 bs2) =
    let sortBindings :: [Binding r] -> [Binding r]
        sortBindings = sortOn (() <$)
        filterBindings :: [Binding r] -> [Binding r] -> [Binding r]
        filterBindings as bs =
          let simplifyName :: NAttrPath a -> NAttrPath ()
              simplifyName = fmap (() <$)
              names        = [ simplifyName p | NamedVar p _ _ <- as ]
          in  [ b | b@(NamedVar p _ _) <- bs, simplifyName p `elem` names ]
        -- bs1' = sortBindings (filterBindings bs2 bs1)
        bs1' = sortBindings bs1
        bs2' = sortBindings (filterBindings bs1 bs2)
    in  to1 <$> gZipMatch (from1 (NSet t1 bs1')) (from1 (NSet t2 bs2'))
  zipMatch (NLet bs1 e1) (NLet bs2 e2) =
    let sortBindings :: [Binding r] -> [Binding r]
        sortBindings = sortOn (() <$)
        filterBindings :: [Binding r] -> [Binding r] -> [Binding r]
        filterBindings as bs =
          let simplifyName :: NAttrPath a -> NAttrPath ()
              simplifyName = fmap (() <$)
              names        = [ simplifyName p | NamedVar p _ _ <- as ]
          in  [ b | b@(NamedVar p _ _) <- bs, simplifyName p `elem` names ]
        -- bs1' = sortBindings (filterBindings bs2 bs1)
        bs1' = sortBindings bs1
        bs2' = sortBindings (filterBindings bs1 bs2)
    in  to1 <$> gZipMatch (from1 (NLet bs1' e1)) (from1 (NLet bs2' e2))
  zipMatch l r = to1 <$> gZipMatch (from1 l) (from1 r)

--
-- hnix types
--

instance Unifiable NString where
  zipMatch = zipMatchViaGeneric

instance Unifiable (Antiquoted Text) where
  zipMatch = zipMatchViaGeneric

-- | The matched pair uses the source location of the first argument
instance Unifiable Binding where
  zipMatch (NamedVar p1 v1 l) (NamedVar p2 v2 _) = do
    p <- zipMatch2 p1 p2
    pure (NamedVar p (Right (v1, v2)) l)
  zipMatch (Inherit x1 ys1 l) (Inherit x2 ys2 _) = do
    x  <- zipMatch x1 x2
    ys <- zipMatch2 ys1 ys2
    pure (Inherit x ys l)
  zipMatch _ _ = Nothing

-- | No Generic1 instance
instance Unifiable NKeyName where
  zipMatch (StaticKey k1) (StaticKey k2) | k1 == k2 = Just (StaticKey k1)
  zipMatch (DynamicKey EscapedNewline) (DynamicKey EscapedNewline) =
    Just (DynamicKey EscapedNewline)
  zipMatch (DynamicKey (Plain k1)) (DynamicKey (Plain k2)) = do
    k <- zipMatch k1 k2
    pure $ DynamicKey (Plain k)
  zipMatch (DynamicKey (Antiquoted k1)) (DynamicKey (Antiquoted k2)) =
    pure $ DynamicKey (Antiquoted (Right (k1, k2)))
  zipMatch _ _ = Nothing

instance Unifiable Params where
  zipMatch = zipMatchViaGeneric

-- | The returned expression uses the annotation combined as some 'Semigroup'
instance Semigroup ann => Unifiable (Ann ann) where
  zipMatch (Ann ann1 a1) (Ann ann2 a2) =
    Just $ Ann (ann1 <> ann2) (Right (a1, a2))

--
-- base types
--

instance Unifiable [] where
  zipMatch = zipMatchViaGeneric

instance Unifiable NonEmpty where
  zipMatch = zipMatchViaGeneric

instance Unifiable Maybe where
  zipMatch = zipMatchViaGeneric

instance Eq a => Unifiable ((,) a) where
  zipMatch = zipMatchViaGeneric

instance (Unifiable f, Unifiable g )=> Unifiable (Compose f g) where
  zipMatch = zipMatchViaGeneric

--
-- helpers
--

-- | Match a composition of 'Unifiable' things
zipMatch2
  :: (Unifiable f, Unifiable t)
  => t (f a)
  -> t (f a)
  -> Maybe (t (f (Either a (a, a))))
zipMatch2 a b = zipMatch a b >>= traverse
  (\case
    Left  x      -> pure $ Left <$> x
    Right (x, y) -> zipMatch x y
  )

----------------------------------------------------------------
-- Generic Instance for Unifiable
----------------------------------------------------------------

-- | Take the generic representation, zipmatch and coerce back
zipMatchViaGeneric
  :: (Generic1 t, GUnifiable (Rep1 t))
  => t a
  -> t a
  -> Maybe (t (Either a (a, a)))
zipMatchViaGeneric l r = to1 <$> gZipMatch (from1 l) (from1 r)

class (Traversable t, Generic1 t) => GUnifiable t where
  gZipMatch :: t a -> t a -> Maybe (t (Either a (a,a)))

instance GUnifiable t => GUnifiable (M1 m i t) where
  gZipMatch (M1 l) (M1 r) = M1 <$> gZipMatch l r

instance GUnifiable U1 where
  gZipMatch _ _ = Just U1

instance Eq c => GUnifiable (K1 m c) where
  gZipMatch (K1 l) (K1 r) | l == r    = Just (K1 l)
                          | otherwise = Nothing

instance GUnifiable Par1 where
  gZipMatch (Par1 l) (Par1 r) = Just . Par1 . Right $ (l, r)

instance Unifiable x => GUnifiable (Rec1 x) where
  gZipMatch (Rec1 l) (Rec1 r) = Rec1 <$> zipMatch l r

instance (GUnifiable l, GUnifiable r) => GUnifiable (l :+: r) where
  gZipMatch (L1 l) (L1 r) = L1 <$> gZipMatch l r
  gZipMatch (R1 l) (R1 r) = R1 <$> gZipMatch l r
  gZipMatch _      _      = Nothing

instance (GUnifiable l, GUnifiable r) => GUnifiable (l :*: r) where
  gZipMatch (l1 :*: r1) (l2 :*: r2) =
    (:*:) <$> gZipMatch l1 l2 <*> gZipMatch r1 r2

instance (Unifiable a, GUnifiable b) => GUnifiable (a :.: b) where
  gZipMatch (Comp1 l) (Comp1 r) = do
    x <- zipMatch l r >>= traverse
      (\case
        Left  a        -> Just $ Left <$> a
        Right (a1, a2) -> gZipMatch a1 a2
      )
    pure (Comp1 x)
