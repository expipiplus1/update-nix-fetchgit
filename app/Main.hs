{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Text.IO        (readFile, writeFile)
import           Prelude             hiding (readFile, writeFile)
import           System.Environment  (getArgs)
import           Update.Nix.FetchGit
import           Update.Span

main :: IO ()
main =
  -- Super simple command line parsing at the moment, just look for one
  -- filename
  getArgs >>= \case
    [filename] ->
      -- Get the updates from this file
      updatesFromFile filename >>= \case
        -- If we have any errors print them and finish
        Left ws  -> print ws
        Right us -> do
          -- TODO: Avoid loading this file twice
          t <- readFile filename
          -- Try to update this text
          case updateSpans us t of
            Nothing -> putStrLn "Error: overlapping updates"
            -- If updates are needed, write to the file.
            Just t' | t' /= t -> writeFile filename t'
            Just _ -> return ()
    _ -> putStrLn "Usage: update-nix-fetchgit filename"

