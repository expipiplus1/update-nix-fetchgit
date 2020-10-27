{-# LANGUAGE QuasiQuotes #-}

module Update.Nix.FetchGit
  ( processFile
  , processText
  , updatesFromText
  ) where

import           Data.Fix
import           Data.Foldable
import           Data.Maybe
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO
import           Data.Time                      ( Day )
import qualified Data.Vector                   as V
import           Nix.Comments
import           Nix.Expr
import           Nix.Match.Typed
import           Say
import           System.Exit
import           System.IO
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Utils
import           Update.Nix.FetchGit.Warning
import           Update.Nix.Updater
import           Update.Span


--------------------------------------------------------------------------------
-- Tying it all together
--------------------------------------------------------------------------------

-- | Provided FilePath, update Nix file in-place
processFile :: FilePath -> IO ()
processFile filename = do
  t  <- Data.Text.IO.readFile filename
  t' <- processText t
  if t == t'
    then sayErr "No updates"
    else do
      -- If updates are needed, write to the file.
      Data.Text.IO.writeFile filename t'

processText :: Text -> IO Text
processText t = runM (updatesFromText t) >>= \case
  -- If we have any errors, print them and finish.
  Left  ws -> printErrorAndExit ws
  -- Update the text of the file in memory.
  Right us -> do
    sayErrString $ "Made " ++ show (length us) ++ " changes"
    pure $ updateSpans us t
 where
  printErrorAndExit :: [Warning] -> IO a
  printErrorAndExit e = do
    traverse_ (hPutStrLn stderr . formatWarning) e
    exitFailure

-- | Given the path to a Nix file, returns the SpanUpdates
-- all the parts of the file we want to update.
updatesFromText :: Text -> M [SpanUpdate]
updatesFromText t = do
  let nixLines = V.fromList (T.lines t)
      getComment sourceLines =
        annotation . getCompose . unFix . annotateWithComments sourceLines
  tree <- do
    expr <- fromEither $ ourParseNixText t
    findUpdates (getComment nixLines) expr
  evalUpdates tree

----------------------------------------------------------------
-- Finding updates
----------------------------------------------------------------

findUpdates :: (NExprLoc -> Maybe Comment) -> NExprLoc -> M FetchTree
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

evalUpdates :: FetchTree -> M [SpanUpdate]
evalUpdates = fmap snd . go
 where
  go :: FetchTree -> M (Maybe Day, [SpanUpdate])
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
