{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Foldable       (foldl')
import           Data.Text.IO
import           Prelude             hiding (readFile, writeFile)
import           Update.Nix.FetchGit
import           Update.Span

main :: IO ()
main = do
  let filename = "test.nix"
  updatesFromFile filename >>= \case
    Left ws  -> print ws
    Right us -> do
      let fs = updateSpan <$> us
          f = foldl' (.) id fs
      fileText <- readFile filename
      let fileText' = f fileText
      writeFile "test2.nix" fileText'

