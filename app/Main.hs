{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Text.IO        (readFile, writeFile)
import           Prelude             hiding (readFile, writeFile)
import           System.Environment  (getArgs)
import           System.Exit
import           Update.Nix.FetchGit
import           Update.Nix.FetchGit.Warning
import           Update.Span

main :: IO ()
main =
  -- Super simple command line parsing at the moment, just look for one
  -- filename.
  getArgs >>= \case
    [filename] -> do
      t <- readFile filename
      -- Get the updates from this file.
      updatesFromFile t >>= \case
        -- If we have any errors, print them and finish.
        Left ws -> printErrorAndExit ws
        Right us -> do
          -- Update the text of the file in memory.
          case updateSpans us t of
            -- If updates are needed, write to the file.
            t' | t' /= t -> writeFile filename t'
            _ -> return ()
    _ -> do
      putStrLn "Usage: update-nix-fetchgit filename"
      exitWith (ExitFailure 1)

printErrorAndExit :: Warning -> IO ()
printErrorAndExit e = do
  print e
  exitWith (ExitFailure 1)
