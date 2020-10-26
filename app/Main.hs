{-# LANGUAGE LambdaCase #-}

module Main where

import qualified System.Environment
import qualified System.Exit
import qualified Update.Nix.FetchGit

main :: IO ()
main =
  -- Super simple command line parsing at the moment, just look for one
  -- filename and optionally pass extra arguments to `nix-prefetch-git`.
       System.Environment.getArgs >>= \case
  [filename] -> Update.Nix.FetchGit.processFile filename
  _          -> do
    putStrLn "Usage: update-nix-fetchgit filename"
    System.Exit.exitFailure
