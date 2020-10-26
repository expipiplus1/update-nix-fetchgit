{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Update.Nix.FetchGit
  ( updatesFromFile
  , processFile
  ) where

import           Control.Monad.Except
import           Data.Foldable                  ( asum
                                                , toList
                                                )
import           Data.Maybe
import           Data.Text                      ( pack
                                                )
import           Nix.Comments
import           Nix.Expr
import           Nix.Match.Typed
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Utils
import           Update.Nix.FetchGit.Warning
import           Update.Nix.Updater
import           Update.Span

import           Data.Fix
import qualified Data.Text                     as T
import qualified Data.Text.IO
import qualified Data.Text.IO                  as T
import qualified Data.Vector                   as V
import qualified System.Exit
import qualified System.IO
import Data.Time (Day)


--------------------------------------------------------------------------------
-- Tying it all together
--------------------------------------------------------------------------------

-- | Provided FilePath, update Nix file in-place
processFile :: FilePath -> IO ()
processFile filename = do
  t <- Data.Text.IO.readFile filename
  -- Get the updates from this file.
  updatesFromFile filename >>= \case
    -- If we have any errors, print them and finish.
    Left  ws -> printErrorAndExit ws
    -- Update the text of the file in memory.
    Right us -> case updateSpans us t of
      t' | t' /= t -> do
        -- If updates are needed, write to the file.
        Data.Text.IO.writeFile filename t'
        putStrLn $ "Made " ++ show (length us) ++ " changes"
      _ -> putStrLn "No updates"
 where
  printErrorAndExit :: Warning -> IO ()
  printErrorAndExit e = do
    System.IO.hPutStrLn System.IO.stderr (formatWarning e)
    System.Exit.exitFailure

-- | Given the path to a Nix file, returns the SpanUpdates
-- all the parts of the file we want to update.
updatesFromFile :: FilePath -> IO (Either Warning [SpanUpdate])
updatesFromFile f = runExceptT $ do
  t <- liftIO $ T.readFile f
  let nixLines = V.fromList (T.lines t)
      getComment sourceLines =
        annotation . getCompose . unFix . annotateWithComments sourceLines
  tree <- ExceptT . pure $ do
    expr <- ourParseNixText t
    findUpdates (getComment nixLines) expr
  evalUpdates tree

----------------------------------------------------------------
-- Finding updates
----------------------------------------------------------------

findUpdates
  :: (NExprLoc -> Maybe Comment) -> NExprLoc -> Either Warning FetchTree
findUpdates getComment e =
  let updaters = (\u -> u getComment e) <$> fetchers
  in
    case asum updaters of
      Just u  -> UpdaterNode <$> u
      Nothing -> case e of
        [matchNixLoc|{ _version = ^version; }|] ->
          Node version <$> traverse (findUpdates getComment) (toList (unFix e))
        _ ->
          Node Nothing <$> traverse (findUpdates getComment) (toList (unFix e))

evalUpdates :: FetchTree -> ExceptT Warning IO [SpanUpdate]
evalUpdates = fmap snd . go
 where
  go :: FetchTree -> ExceptT Warning IO (Maybe Day, [SpanUpdate])
  go = \case
    UpdaterNode (Updater u) -> u
    Node versionExpr cs     -> do
      (ds, ss) <- unzip <$> traverse go cs
      -- Update version string with the maximum of versions in the children
      let latestDate = maximumMay (catMaybes ds)
      pure
        ( latestDate
        , [ SpanUpdate (exprSpan v) (quoteString . pack . show $ d)
          | Just d <- pure latestDate
          , Just v <- pure versionExpr
          ]
          <> concat ss
        )

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

maximumMay :: Ord a => [a] -> Maybe a
maximumMay = \case
  [] -> Nothing
  xs -> Just (maximum xs)
