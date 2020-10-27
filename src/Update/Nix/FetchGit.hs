{-# LANGUAGE QuasiQuotes #-}

module Update.Nix.FetchGit
  ( processFile
  , processText
  , updatesFromText
  ) where

import           Control.Monad                  ( when )
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
import           System.Exit
import           Update.Nix.FetchGit.Types
import           Update.Nix.FetchGit.Utils
import           Update.Nix.Updater
import           Update.Span
import           Control.Monad.Validate         ( MonadValidate(tolerate) )
import           Data.Functor


--------------------------------------------------------------------------------
-- Tying it all together
--------------------------------------------------------------------------------

-- | Provided FilePath, update Nix file in-place
processFile :: Env -> FilePath -> IO ()
processFile env filename = do
  t  <- Data.Text.IO.readFile filename
  t' <- processText env t
  -- If updates are needed, write to the file.
  when (t /= t') $ Data.Text.IO.writeFile filename t'

processText :: Env -> Text -> IO Text
processText env t = do
  (es, t') <- runM env (updatesFromText t <&> (`updateSpans` t))
  traverse_ (sayLog env Normal . formatWarning) es
  maybe exitFailure pure t'

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
  us <- evalUpdates tree
  case us of
    []  -> logVerbose "Made no updates"
    [_] -> logVerbose "Made 1 update"
    _   -> logVerbose ("Made " <> T.pack (show (length us)) <> " updates")
  pure us

----------------------------------------------------------------
-- Finding updates
----------------------------------------------------------------

findUpdates :: (NExprLoc -> Maybe Comment) -> NExprLoc -> M FetchTree
findUpdates getComment e =
  let updaters = ($ e) <$> fetchers getComment
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
      (ds, ss) <- unzip . catMaybes <$> traverse (tolerate . go) cs
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
