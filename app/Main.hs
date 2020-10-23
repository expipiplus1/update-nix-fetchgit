{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Text
import qualified System.Environment
import qualified System.Exit
import qualified Update.Nix.FetchGit

import Update.Nix.Match

main = test

-- main :: IO ()
-- main =
--   -- Super simple command line parsing at the moment, just look for one
--   -- filename and optionally pass extra arguments to `nix-prefetch-git`.
--   System.Environment.getArgs >>= \case
--     [filename]      -> Update.Nix.FetchGit.processFile filename []
--     (filename:args) -> Update.Nix.FetchGit.processFile filename (map Data.Text.pack args)
--     _ -> do
--       putStrLn "Usage: update-nix-fetchgit filename [<extra-prefetch-args>]"
--       System.Exit.exitFailure
