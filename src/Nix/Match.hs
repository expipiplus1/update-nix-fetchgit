{-# LANGUAGE UndecidableInstances #-}

-- | A set of functions for matching on Nix expression trees and extracting the
-- values of sub-trees.
module Nix.Match
  ( match,
    findMatches,
    Matchable (..),
    GMatchable (..),
    WithHoles (..),
    addHoles,
    addHolesLoc,
    isOptionalPath,
  )
where

import Control.Category ((>>>))
import Control.Monad (void)
import Data.Data
import Data.Fix
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Monoid hiding (All)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Base (NonEmpty ((:|)))
import GHC.Generics
import Nix

-- | Like 'Fix' but each layer could instead be a 'Hole'
data WithHoles t v
  = Hole !v
  | Term !(t (WithHoles t v))

deriving instance (Typeable t, Data (t (WithHoles t v)), Data v) => Data (WithHoles t v)

-- | Match a tree with holes against a tree without holes, returning the values
-- of the holes if it matches.
--
-- 'NExprF' and 'NExprLocF' are both instances of 'Matchable'. 'NExprLocF' does
-- not require the annotations to match. Please see the 'Matchable' instance
-- documentation for 'NExprF' for more details.
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
--
-- >>> import Nix.TH
-- >>> import Control.Arrow
-- >>> pretty = prettyNix *** (fmap @[] (fmap @((,) Text) prettyNix))
-- >>> pretty <$> findMatches (addHoles [nix|{x=^x;}|]) [nix|{x=1;a={x=2;};}|]
-- [({ x = 1; a = { x = 2; }; },[("x",1)]),({ x = 2; },[("x",2)])]
findMatches ::
  Matchable t =>
  -- | Needle
  WithHoles t v ->
  -- | Haystack
  Fix t ->
  [(Fix t, [(v, Fix t)])]
findMatches needle haystack =
  [(s, r) | s <- fixUniverse haystack, Just r <- pure $ match needle s]

-- | Get every @f@ in a @Fix f@ in preorder.
fixUniverse :: Foldable f => Fix f -> [Fix f]
fixUniverse e = e : (fixUniverse =<< toList (unFix e))

-- | Make syntactic holes into 'Hole's
addHoles :: NExpr -> WithHoles NExprF VarName
addHoles =
  unFix >>> \case
    NSynHole n -> Hole n
    e -> Term . fmap addHoles $ e

-- | Make syntactic holes into 'Hole's
addHolesLoc :: NExprLoc -> WithHoles NExprLocF VarName
addHolesLoc =
  unFix >>> \case
    Compose (AnnUnit _ (NSynHole n)) -> Hole n
    e -> Term . fmap addHolesLoc $ e

----------------------------------------------------------------
-- Matchable
----------------------------------------------------------------

-- | Instances for this class can be derived for any type with a 'Generic1'
-- instance.
class Traversable t => Matchable t where
  -- | Match one level of structure, returning the matched structure with sub
  -- structures to match. Needle is the first argument, matchee is the second.
  --
  -- Unlike the @Unifiable@ class in the "unification-fd" package, this doesn't
  -- have to be a commutative operation, the needle will always be the first
  -- parameter and instances are free to treat if differently if appropriate.
  zipMatchLeft :: t a -> t b -> Maybe (t (a, b))
  default zipMatchLeft ::
    (Generic1 t, GMatchable (Rep1 t)) =>
    t a ->
    t b ->
    Maybe (t (a, b))
  zipMatchLeft l r = to1 <$> gZipMatchLeft (from1 l) (from1 r)

-- | Match a composition of 'Matchable' things
zipMatchLeft2 ::
  (Matchable f, Matchable t) => t (f a) -> t (f b) -> Maybe (t (f (a, b)))
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
-- - For attrsets and let bindings, bindings which have a LHS beginning with
--   @_@ are treated as optional. If they are not present then any holes on
--   their RHS will not be filled.
--
-- - Attrsets match ignoring recursiveness
--
-- - If a function in the needle has @_@ as its parameter, it matches
--   everything, so @_@ acts as a wildcard pattern.
instance Matchable NExprF where
  zipMatchLeft (NSet _ bs1) (NSet _ bs2) = do
    (bs1', bs2') <- unzip <$> reduceBindings bs1 bs2
    to1
      <$> gZipMatchLeft
        (from1 (NSet NonRecursive bs1'))
        (from1 (NSet NonRecursive bs2'))
  zipMatchLeft (NLet bs1 e1) (NLet bs2 e2) = do
    (bs1', bs2') <- unzip <$> reduceBindings bs1 bs2
    to1 <$> gZipMatchLeft (from1 (NLet bs1' e1)) (from1 (NLet bs2' e2))
  zipMatchLeft (NAbs (Param "_") e1) (NAbs _ e2) = do
    pure $ NAbs (Param "_") (e1, e2)
  zipMatchLeft l r = to1 <$> gZipMatchLeft (from1 l) (from1 r)

-- | Bindings are compared on top level structure only.
--
-- Doesn't filter bindings in the needle, as they must all be present
--
-- Bindings are returned according to their order in the needle.
--
-- Any optional (name begins with @_@) bindings may be removed from the needle.
--
-- Left hand sides are matched purely on the top level structure, this means
-- that "${a}" and "${b}" appear the same to this function, and it may not
-- match them up correctly.
reduceBindings :: [Binding q] -> [Binding r] -> Maybe [(Binding q, Binding r)]
reduceBindings needle matchee =
  let -- A binding is optional if the lhs starts with a '_', return the same
      -- binding but without the '_'
      isOptional = \case
        NamedVar p e l | Just p' <- isOptionalPath p -> Just (NamedVar p' e l)
        _ -> Nothing

      -- Get a representation of the left hand side which has an Eq instance
      -- This will represent some things the samelike "${a}" and "${b}"
      getLHS = \case
        NamedVar p _ _ -> Left (fmap void p)
        Inherit r ps _ -> Right (void r, ps)
   in sequence
        [ (n',) <$> m
          | -- For each binding in the needle
            n <- needle,
            let opt = isOptional n
                -- \| Use the optional demangled version if present
                n' = fromMaybe n opt
                lhs = getLHS n'
                -- Find the first matching binding in the matchee
                m = find ((lhs ==) . getLHS) matchee,
            -- Skip this element if it is not present in the matchee and is optional in the needle
            isNothing opt || isJust m
        ]

-- | Basically: does the path begin with an underscore, if so return it removed
-- without the underscore.
isOptionalPath :: NAttrPath r -> Maybe (NAttrPath r)
isOptionalPath = \case
  StaticKey (VarName n) :| [] | Just ('_', t) <- T.uncons n -> Just (StaticKey (VarName t) :| [])
  DynamicKey (Plain (DoubleQuoted [Plain n])) :| rs
    | Just ('_', t) <- T.uncons n ->
        Just
          (DynamicKey (Plain (DoubleQuoted [Plain t])) :| rs)
  _ -> Nothing

--
-- hnix types
--

instance Matchable NString

instance Matchable (Antiquoted Text)

-- | The matched pair uses the source location of the first argument
instance Matchable Binding where
  zipMatchLeft (NamedVar p1 v1 _) (NamedVar p2 v2 l) = do
    p <- zipMatchLeft2 p1 p2
    pure (NamedVar p (v1, v2) l)
  zipMatchLeft (Inherit x1 ys1 l) (Inherit x2 ys2 _)
    | ys1 == ys2 = do
        x <- zipMatchLeft x1 x2
        pure (Inherit x ys1 l)
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

instance Matchable Params

-- | Doesn't require the annotations to match, returns the second annotation.
instance Matchable (AnnUnit ann) where
  zipMatchLeft (AnnUnit _ a1) (AnnUnit ann2 a2) = Just $ AnnUnit ann2 (a1, a2)

--
-- base types
--

instance Matchable []

instance Matchable NonEmpty

instance Matchable Maybe

instance Eq a => Matchable ((,) a)

instance (Matchable f, Matchable g) => Matchable (Compose f g)

----------------------------------------------------------------
-- Generic Instance for Matchable
----------------------------------------------------------------

-- | A class used in the @default@ definition for 'zipMatchLeft'
class (Traversable t, Generic1 t) => GMatchable t where
  gZipMatchLeft :: t a -> t b -> Maybe (t (a, b))

instance GMatchable t => GMatchable (M1 m i t) where
  gZipMatchLeft (M1 l) (M1 r) = M1 <$> gZipMatchLeft l r

instance GMatchable U1 where
  gZipMatchLeft _ _ = Just U1

instance Eq c => GMatchable (K1 m c) where
  gZipMatchLeft (K1 l) (K1 r)
    | l == r = Just (K1 l)
    | otherwise = Nothing

instance GMatchable Par1 where
  gZipMatchLeft (Par1 l) (Par1 r) = Just . Par1 $ (l, r)

instance Matchable x => GMatchable (Rec1 x) where
  gZipMatchLeft (Rec1 l) (Rec1 r) = Rec1 <$> zipMatchLeft l r

instance (GMatchable l, GMatchable r) => GMatchable (l :+: r) where
  gZipMatchLeft (L1 l) (L1 r) = L1 <$> gZipMatchLeft l r
  gZipMatchLeft (R1 l) (R1 r) = R1 <$> gZipMatchLeft l r
  gZipMatchLeft _ _ = Nothing

instance (GMatchable l, GMatchable r) => GMatchable (l :*: r) where
  gZipMatchLeft (l1 :*: l2) (r1 :*: r2) =
    (:*:) <$> gZipMatchLeft l1 r1 <*> gZipMatchLeft l2 r2

instance (Matchable a, GMatchable b) => GMatchable (a :.: b) where
  gZipMatchLeft (Comp1 l) (Comp1 r) = do
    x <- zipMatchLeft l r >>= traverse (uncurry gZipMatchLeft)
    pure (Comp1 x)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)
