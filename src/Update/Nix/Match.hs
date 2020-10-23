{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Update.Nix.Match where

import           Data.Text                      ( Text )
import           Nix

import           Control.Error                  ( ExceptT
                                                , runExceptT
                                                )
import           Control.Unification            ( BindingMonad(freeVar)
                                                , UTerm(..)
                                                , freeze
                                                , unify
                                                )
import           Control.Unification.IntVar     ( IntBindingT
                                                , IntVar
                                                , runIntBindingT
                                                )
import           Control.Unification.Types      ( UFailure )
import           Data.Fix                       ( Fix(..)
                                                , unfoldFix
                                                )
import qualified Data.Functor.Fixedpoint

import           Control.Category               ( (>>>) )
import           Control.DeepSeq
import           Control.Exception              ( evaluate )
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , evalState
                                                , get
                                                , gets
                                                , modify'
                                                )
import           Control.Unification.Ranked     ( applyBindings )
import           Data.Coerce
import           Data.Foldable
import           Data.Semigroup                 ( First(First) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Nix.TH                         ( nix )
import           Say
import           Unsafe.Coerce                  ( unsafeCoerce )

import           Update.Nix.Unify               ( )
import           Debug.Trace                    ( traceShowId )

test :: IO ()
test = do
  let needle = annotateNullSourcePos [nix|
        mkDerivation {
          pname = ^name;
          version = "1.0.1";
        }
      |]
  sayErr "Parsing"
  Success haystack <-
    parseNixFileLoc
      "/home/j/src/nixpkgs/pkgs/development/haskell-modules/hackage-packages.nix"
  sayErr "Parsed"
  Control.Exception.evaluate . Control.DeepSeq.force $ haystack
  sayErr "Evaluated"
  let matches = findExpr needle haystack
      tags =
        [ t <> ": " <> str
        | (_, ms) <- matches
        , (t, e ) <- ms
        , let str = case e of
                Nothing -> "Not Found"
                Just x  -> T.pack . show . prettyNix . stripAnnotation $ x
        ]
  traverse_ T.putStrLn tags

fixUniverse :: Foldable f => Fix f -> [Fix f]
fixUniverse e = e : (fixUniverse =<< toList (unFix e))

type E = AnnF (First SrcSpan) NExprF
type B = IntBindingT E (State [(Text, IntVar)])
type M = ExceptT (UFailure E IntVar) B

findExpr :: NExprLoc -> NExprLoc -> [(NExprLoc, [(Text, Maybe NExprLoc)])]
findExpr needle haystack =
  [ (s, r) | s <- fixUniverse haystack, Right r <- pure $ matchExpr needle s ]

matchExpr
  :: NExprLoc -> NExprLoc -> Either (UFailure E IntVar) [(Text, Maybe NExprLoc)]
matchExpr e1 e2 =
  fst . flip evalState [] . runIntBindingT . runExceptT $ matchExpr' e1 e2

matchExpr' :: NExprLoc -> NExprLoc -> M [(Text, Maybe NExprLoc)]
matchExpr' e1 e2 = do
  t1 <- lift $ makeTerm (mapAnn @_ @(First SrcSpan) e1)
  t2 <- lift $ makeTerm (mapAnn @_ @(First SrcSpan) e2)
  unify t1 t2
  lift (lift get) >>= traverse
    (\(n, i) -> do
      v <- freeze <$> applyBindings (UVar i)
      pure (n, mapAnn @_ @SrcSpan . fix2Fix <$> v)
    )

makeTerm :: Fix E -> B (UTerm E IntVar)
makeTerm = unFix >>> \case
  Compose (Ann _ (NSynHole n)) -> do
    v <- lift (gets (lookup n)) >>= \case
      Nothing -> do
        v <- freeVar
        lift (modify' ((n, v) :))
        pure v
      Just v -> pure v
    pure (UVar v)
  e -> fmap UTerm . traverse makeTerm $ e

fix2Fix :: Functor f => Data.Functor.Fixedpoint.Fix f -> Data.Fix.Fix f
fix2Fix = unfoldFix Data.Functor.Fixedpoint.unFix

mapAnn :: Coercible a b => Fix (AnnF a NExprF) -> Fix (AnnF b NExprF)
mapAnn = unsafeCoerce

annotateNullSourcePos :: NExpr -> NExprLoc
annotateNullSourcePos =
  Fix
    . Compose
    . fmap (fmap annotateNullSourcePos)
    . Ann (SrcSpan nullSourcePos nullSourcePos)
    . unFix

nullSourcePos :: SourcePos
nullSourcePos = SourcePos "" (mkPos 1) (mkPos 1)

