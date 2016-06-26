{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Text.IO        (readFile, writeFile)
import           Prelude             hiding (readFile, writeFile)
import           System.Environment  (getArgs)
import           Update.Nix.FetchGit
import           Update.Span

main :: IO ()
main =
  getArgs >>= \case
    [filename] ->
      updatesFromFile filename >>= \case
        Left ws  -> print ws
        Right us -> do
          t <- readFile filename
          case updateSpans us t of
            Nothing -> putStrLn "Error: overlapping updates"
            Just t' -> writeFile filename t'
    _ -> putStrLn "Usage: update-nix-fetchgit filename"

