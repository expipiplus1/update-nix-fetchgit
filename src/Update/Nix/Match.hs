{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Update.Nix.Match where

import           Control.DeepSeq
import           Control.Exception              ( evaluate )
import           Data.Fix                       ( Fix(..) )
import           Data.Foldable
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Nix
import           Nix.Match.Typed
import           Say

test :: IO ()
test = do
  let needle = [matchNixLoc|
        _.mkDerivation {
          version = ^version;
          pname = ^name;
          _broken = ^broken;
        }
      |]
  sayErr "Parsing"
  Success haystack <-
    parseNixFileLoc
      "/home/j/src/nixpkgs/pkgs/development/haskell-modules/hackage-packages.nix"
  sayErr "Parsed"
  _ <- Control.Exception.evaluate . Control.DeepSeq.force $ haystack
  sayErr "Evaluated"
  let matches = findMatchesTyped needle haystack
      str     = T.pack . show . prettyNix . stripAnnotation
      tags =
        [ str (get @"name" ms) <> ":" <> str (get @"version" ms) <> " " <> maybe
            "false"
            str
            (getOptional @"broken" ms)
        | (_, ms) <- matches
        ]
  traverse_ T.putStrLn tags

annotateNullSourcePos :: NExpr -> NExprLoc
annotateNullSourcePos =
  Fix
    . Compose
    . fmap (fmap annotateNullSourcePos)
    . Ann (SrcSpan nullSourcePos nullSourcePos)
    . unFix

nullSourcePos :: SourcePos
nullSourcePos = SourcePos "" (mkPos 1) (mkPos 1)

foo
  :: NExprLoc -> (NExprLoc, NExprLoc)
foo (fmap (\x -> (get @"version" x, get @"name" x)) . matchTyped needle' -> Just (version,name))
  = (version, name)

bar [matchNixLoc|
  mkDerivation {
    version = ^version;
    pname = ^name;
    _broken = x;
  }
|] = (version, name)

needle' :: TypedMatcher '["broken"] '["version", "name"] NExprLocF
needle' = [matchNixLoc|
  mkDerivation {
    version = ^version;
    pname = ^name;
    _broken = ^broken;
  }
|]
