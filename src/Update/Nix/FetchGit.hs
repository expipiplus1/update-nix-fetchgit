{-# LANGUAGE QuasiQuotes #-}

module Update.Nix.FetchGit
  ( processFile,
    processText,
    updatesFromText,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Validate (MonadValidate (tolerate))
import Data.Fix
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Text
  ( Text,
    pack,
  )
import qualified Data.Text as T
import qualified Data.Text.IO
import Data.Time (Day)
import qualified Data.Vector as V
import Nix.Comments
import Nix.Expr
import Nix.Match.Typed
import System.Exit
import Text.Regex.TDFA
import Update.Nix.FetchGit.Types
import Update.Nix.FetchGit.Utils
import Update.Nix.Updater
import Update.Span

--------------------------------------------------------------------------------
-- Tying it all together
--------------------------------------------------------------------------------

-- | Provided FilePath, update Nix file in-place
processFile :: Env -> FilePath -> IO ()
processFile env filename = do
  t <- Data.Text.IO.readFile filename
  t' <- processText env t
  -- If updates are needed, write to the file.
  when (t /= t') $ Data.Text.IO.writeFile filename t'

processText :: Env -> Text -> IO Text
processText env t = do
  (es, t') <- runM env (updatesFromText t <&> (`updateSpans` t))
  traverse_ (sayLog env Normal . formatWarning) es
  maybe exitFailure pure $ case dryness env of
    Wet -> t'
    Dry -> Just t

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
  us <- evalUpdates =<< filterUpdates tree
  case us of
    [] -> logVerbose "Made no updates"
    [_] -> logVerbose "Made 1 update"
    _ -> logVerbose ("Made " <> T.pack (show (length us)) <> " updates")
  pure us

----------------------------------------------------------------
-- Finding updates
----------------------------------------------------------------

findUpdates :: (NExprLoc -> Maybe Comment) -> NExprLoc -> M FetchTree
findUpdates getComment e = do
  Env {..} <- ask
  -- First of all, if this expression doesn't enclose the requested position,
  -- return an empty tree
  -- Then check against all the updaters, if they match we have a leaf
  if not (null updateLocations || any (containsPosition e) updateLocations)
    then pure $ Node Nothing []
    else
      let updaters = ($ e) <$> fetchers onlyCommented getComment
          bindingTrees = \case
            NamedVar p e' _
              | Just t <- pathText p ->
                  (: []) . (Just t,) <$> findUpdates getComment e'
            b ->
              traverse (fmap (Nothing,) . findUpdates getComment) . toList $ b
       in case asum updaters of
            Just u -> UpdaterNode <$> u
            Nothing -> case e of
              [matchNixLoc|{ _version = ^version; }|]
                | NSetAnnF _ _ bs <- unFix e ->
                    Node version . concat <$> traverse bindingTrees bs
              [matchNixLoc|let _version = ^version; in ^x|]
                | NLetAnnF _ bs _ <- unFix e -> do
                    bs' <- concat <$> traverse bindingTrees bs
                    x' <- findUpdates getComment x
                    pure $ Node version ((Nothing, x') : bs')
              _ ->
                Node Nothing
                  <$> traverse
                    (fmap (Nothing,) . findUpdates getComment)
                    (toList (unFix e))

filterUpdates :: FetchTree -> M FetchTree
filterUpdates t = do
  Env {..} <- ask
  let matches s = any (`match` s) attrPatterns
  -- If we're in a branch, include any bindings which match unconditionally,
  -- otherwise recurse
  -- If we reach a leaf, return empty because it hasn't been included by a
  -- binding yet
  let go = \case
        Node v cs ->
          Node
            v
            [ (n, c')
              | (n, c) <- cs,
                let c' = if maybe False matches n then c else go c
            ]
        UpdaterNode _ -> Node Nothing []
  -- If there are no patterns, don't do any filtering
  pure $ if null attrPatterns then t else go t

evalUpdates :: FetchTree -> M [SpanUpdate]
evalUpdates = fmap snd . go
  where
    go :: FetchTree -> M (Maybe Day, [SpanUpdate])
    go = \case
      UpdaterNode (Updater u) -> u
      Node versionExpr cs -> do
        -- Run over all children
        (ds, ss) <- unzip . catMaybes <$> traverse (tolerate . go . snd) cs
        -- Update version string with the maximum of versions in the children
        let latestDate = maximumMay (catMaybes ds)
        pure
          ( latestDate,
            [ SpanUpdate
                (exprSpan v)
                (quoteString . ("unstable-" <>) . pack . show $ d)
              | Just d <- pure latestDate,
                Just v <- pure versionExpr
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
