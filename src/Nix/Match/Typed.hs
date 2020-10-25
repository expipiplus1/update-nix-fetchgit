{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A more strongly typed alternative to 'Nix.Match'
module Nix.Match.Typed
  ( matchNix
  , matchNixLoc
  , TypedMatcher(..)
  , TypedMatch(..)
  , get
  , getOptional
  , matchTyped
  , findMatchesTyped
  ) where

import           Control.Category               ( (>>>) )
import           Data.Coerce                    ( coerce )
import           Data.Data
import           Data.Fix
import           Data.Generics.Aliases
import           Data.Kind                      ( Constraint )
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Type.Equality             ( type (==) )
import           GHC.TypeLits                   ( ErrorMessage(..)
                                                , KnownSymbol
                                                , Symbol
                                                , TypeError
                                                , symbolVal
                                                )
import           Language.Haskell.TH            ( Exp(AppE, VarE)
                                                , ExpQ
                                                , Pat(..)
                                                , PatQ
                                                , Q
                                                , TyLit(StrTyLit)
                                                , Type(..)
                                                , appTypeE
                                                , litT
                                                , mkName
                                                , newName
                                                , strTyLit
                                                , tupE
                                                , tupP
                                                , varE
                                                , varP
                                                )
import           Language.Haskell.TH.Lib        ( appE
                                                , conE
                                                )
import           Language.Haskell.TH.Quote      ( QuasiQuoter(..) )
import           Language.Haskell.TH.Syntax     ( dataToExpQ
                                                , liftString
                                                )
import           Nix                     hiding ( TypeError )
import           Nix.Match
import           Nix.TH

----------------------------------------------------------------
-- Typed matching
----------------------------------------------------------------

-- | A QuasiQuoter for safely generating 'TypedMatcher's from nix source
--
-- The expression has the type @'TypedMatcher' opts reqs 'NExprF'@ where @opts@
-- and @reqs@ are the optional and required holes from the source expression.
--
-- The pattern, if matched, will bring into scope variables named according to
-- the holes present in the expression. These will have type 'NExpr' if they
-- are required, and @Maybe 'NExpr'@ if they are optional.
--
-- >>> case [nix|{a="hello";}|] of [matchNix|{a=^a;}|] -> a
-- Fix (NStr (DoubleQuoted [Plain "hello"]))
--
-- >>> :t [matchNix|{a = ^a; b = {c = ^c; _d = ^d;};}|]
-- [matchNix|{a = ^a; b = {c = ^c; _d = ^d;};}|] :: TypedMatcher '["d"] '["a", "c"] NExprF
--
-- >>> [matchNix|let a = ^a; _b = ^b; in x|] = undefined
-- >>> :t (a, b)
-- (a, b) :: (Fix NExprF, Maybe (Fix NExprF))
matchNix :: QuasiQuoter
matchNix = QuasiQuoter { quoteExp  = typedMatcherExp
                       , quotePat  = typedMatcherPat
                       , quoteDec  = error "No dec quoter for typedMatcher"
                       , quoteType = error "No type quoter for typedMatcher"
                       }

-- | A QuasiQuoter for safely generating 'TypedMatcher's from nix source along
-- with source location annotations
--
-- The expression has the type @'TypedMatcher' opts reqs 'NExprLocF'@ where
-- @opts@ and @reqs@ are the optional and required holes from the source
-- expression.
--
-- The pattern, if matched, will bring into scope variables named according to
-- the holes present in the expression. These will have type 'NExprLoc' if they
-- are required, and @Maybe 'NExprLoc'@ if they are optional.
matchNixLoc :: QuasiQuoter
matchNixLoc = QuasiQuoter
  { quoteExp  = typedMatcherLocExp
  , quotePat  = typedMatcherLocPat
  , quoteDec  = error "No dec quoter for typedMatcherLoc"
  , quoteType = error "No type quoter for typedMatcherLoc"
  }

-- | A matcher with the names of the required and optional holes encoded at the
-- type level.
newtype TypedMatcher (opts :: [Symbol]) (reqs :: [Symbol]) t
  = TypedMatcher {unTypedMatcher :: WithHoles t T.Text}

-- | The results of matching with a 'TypedMatcher'. The values in the required
-- list are guaranteed to be present. The values in the optional list may be
-- present. Use 'get' and 'getOptional' to extract them safely.
newtype TypedMatch (opts :: [Symbol]) (reqs :: [Symbol]) a
  = TypedMatch [(T.Text, a)]

-- | Extract a required key from a match
get
  :: forall x opts reqs a
   . (Elem "Required" x reqs, KnownSymbol x)
  => TypedMatch opts reqs a
  -> a
get (TypedMatch ms) =
  fromMaybe (error "Required key not present in TypedMatch")
    $ lookup (T.pack (symbolVal (Proxy @x))) ms

-- | Maybe extract an optional key from a match
getOptional
  :: forall x opts reqs a
   . (Elem "Optional" x opts, KnownSymbol x)
  => TypedMatch opts reqs a
  -> Maybe a
getOptional (TypedMatch ms) = lookup (T.pack (symbolVal (Proxy @x))) ms

-- | A typed version of 'match'
matchTyped
  :: Matchable t
  => TypedMatcher opts reqs t
  -> Fix t
  -> Maybe (TypedMatch opts reqs (Fix t))
matchTyped = coerce match

-- | A typed version of 'findMatches'
findMatchesTyped
  :: Matchable t
  => TypedMatcher opts reqs t
  -> Fix t
  -> [(Fix t, TypedMatch opts reqs (Fix t))]
findMatchesTyped = coerce findMatches

typedMatcherExp :: String -> ExpQ
typedMatcherExp =
  fmap snd . typedMatcherGen parseNixText collectHoles addHoles id

typedMatcherLocExp :: String -> ExpQ
typedMatcherLocExp =
  fmap snd
    . typedMatcherGen parseNixTextLoc
                      collectHolesLoc
                      addHolesLoc
                      stripAnnotation

typedMatcherPat :: String -> PatQ
typedMatcherPat = typedMatcherPatGen parseNixText collectHoles addHoles id

typedMatcherLocPat :: String -> PatQ
typedMatcherLocPat =
  typedMatcherPatGen parseNixTextLoc collectHolesLoc addHolesLoc stripAnnotation

typedMatcherPatGen
  :: Data a
  => (T.Text -> Result t)
  -> (t -> ([T.Text], [T.Text]))
  -> (t -> a)
  -> (t -> NExpr)
  -> String
  -> Q Pat
typedMatcherPatGen parseNix collect add strip s = do
  ((opt, req), matcher) <- typedMatcherGen parseNix collect add strip s
  -- e' <- [|fmap (\x -> $()) . matchTyped $(pure matcher)|]
  x                     <- newName "x"
  let pat        = tupP (varP . mkName . T.unpack <$> (req <> opt))
      textSymbol = litT . strTyLit . T.unpack
      getters    = tupE
        (  ((\r -> [|get @($r) $(varE x)|]) . textSymbol <$> req)
        <> ((\o -> [|getOptional @($o) $(varE x)|]) . textSymbol <$> opt)
        )
  [p|(fmap (\ $(varP x) -> $getters) . matchTyped $(pure matcher) -> Just $pat)|]

typedMatcherGen
  :: Data a
  => (T.Text -> Result t)
  -> (t -> ([T.Text], [T.Text]))
  -> (t -> a)
  -> (t -> NExpr)
  -> String
  -> Q (([T.Text], [T.Text]), Exp)
typedMatcherGen parseNix collect add strip s = do
  expr <- case parseNix (T.pack s) of
    Failure err -> fail $ show err
    Success e   -> pure e
  let (opt, req) = collect expr
      optT       = symbolList opt
      reqT       = symbolList req
      holed      = add expr
      exprExp    = dataToExpQ
        (      const Nothing
        `extQ` metaExp (freeVars (strip expr))
        `extQ` (Just . liftText)
        )
        holed
  e <-
    conE 'TypedMatcher `appTypeE` pure optT `appTypeE` pure reqT `appE` exprExp
  pure ((opt, req), e)

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> liftString (T.unpack txt)

-- | Make a list of promoted strings
symbolList :: [T.Text] -> Type
symbolList = foldr
  (\n -> (PromotedConsT `AppT` LitT (StrTyLit (T.unpack n)) `AppT`))
  PromotedNilT

-- | Collect optional and required holes
collectHoles :: NExpr -> ([T.Text], [T.Text])
collectHoles = unFix >>> \case
  NSynHole n -> ([], [n])
  NSet _  bs -> foldMap (bindingHoles collectHoles) bs
  NLet bs e  -> collectHoles e <> foldMap (bindingHoles collectHoles) bs
  e          -> foldMap collectHoles e

-- | Collect optional and required holes
collectHolesLoc :: NExprLoc -> ([T.Text], [T.Text])
collectHolesLoc = unFix >>> \case
  Compose (Ann _ (NSynHole n)) -> ([], [n])
  Compose (Ann _ (NSet _ bs )) -> foldMap (bindingHoles collectHolesLoc) bs
  Compose (Ann _ (NLet bs e)) ->
    collectHolesLoc e <> foldMap (bindingHoles collectHolesLoc) bs
  e -> foldMap collectHolesLoc e

-- | Find the optional and required holees in a binding
bindingHoles :: (r -> ([a], [a])) -> Binding r -> ([a], [a])
bindingHoles f = \case
  b@(NamedVar p _ _) | isJust (isOptionalPath p) ->
    let (opt, req) = foldMap f b in (opt <> req, [])
  b -> foldMap f b

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

type family Bool' (f :: k) (t :: k) (x :: Bool) :: k where
  Bool' f _ 'False = f
  Bool' _ t 'True = t

type family Elem n x ys :: Constraint where
  Elem n x '[] = TypeError ('Text n ':<>: 'Text " key \"" ':<>: 'Text x ':<>: 'Text "\" not found in TypedMatch")
  Elem n x (y:ys) = Bool' (Elem n x ys) (() :: Constraint) (x == y)
