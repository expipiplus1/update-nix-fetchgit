{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Update.Nix.FetchGit
  ( updatesFromFile
  ) where

import           Control.Concurrent.Async    (mapConcurrently)
import           Data.Aeson                  (FromJSON, decode)
import           Data.ByteString.Lazy.UTF8   (fromString)
import           Data.Generics.Uniplate.Data
import           Data.Monoid                 ((<>))
import           Data.Text
import qualified Data.Text.IO                as T
import           GHC.Generics
import           Nix.Expr
import           Nix.Parser                  (Result (..), parseNixTextLoc)
import           System.Exit                 (ExitCode (..))
import           System.IO                   (hPutStrLn, stderr)
import           System.Process              (readProcessWithExitCode)
import           Update.Nix.FetchGit.Utils
import           Update.Nix.FetchGit.Warning
import           Update.Span

-- TODO: Remove when using base 4.9
import           Data.Data
import           Data.Functor.Compose
deriving instance (Typeable f, Typeable g, Typeable a, Data (f (g a))) => Data (Compose f g a)

data FetchGitArgs = FetchGitArgs{ urlStringExpr :: NExprLoc
                                , revExpr       :: NExprLoc
                                , sha256Expr    :: NExprLoc
                                }
  deriving (Show)

data FetchGitUpdateInfo = FetchGitUpdateInfo{ urlString :: Text
                                            , revPos    :: SourceSpan
                                            , sha256Pos :: SourceSpan
                                            }
  deriving (Show)

data FetchGitSpanUpdates = FetchGitSpanUpdates{ revUpdate    :: SpanUpdate
                                              , sha256Update :: SpanUpdate
                                              }
  deriving (Show)

data NixPrefetchGitOutput = NixPrefetchGitOutput{ url    :: Text
                                                , rev    :: Text
                                                , sha256 :: Text
                                                }
  deriving (Show, Generic, FromJSON)

updatesFromFile :: FilePath -> IO (Either Warning [SpanUpdate])
updatesFromFile filename = do
  t <- T.readFile filename
  case parseNixTextLoc t of
    Failure d -> pure $ Left (CouldNotParseInput d)
    Success e -> do
      let Right updateInfos = fetchGitUpdateInfos e
      sequenceA <$> mapConcurrently updateInfoToUpdate updateInfos >>= \case
        Left w -> pure $ Left w
        Right pairs -> pure $ Right [u | FetchGitSpanUpdates{..} <- pairs
                                    , u <- [revUpdate, sha256Update]
                                    ]


updateInfoToUpdate :: FetchGitUpdateInfo -> IO (Either Warning FetchGitSpanUpdates)
updateInfoToUpdate ui = do
  o <- nixPrefetchGit (urlString ui)
  pure $ prefetchGitOutputToSpanUpdate ui <$> o

nixPrefetchGit :: Text -> IO (Either Warning NixPrefetchGitOutput)
nixPrefetchGit url = do
  let nixPrefetchCommand = "nix-prefetch-git " ++ unpack url
      nixShellArgs = [ "-p", "nix-prefetch-git"
                     , "-p", "nix"
                     , "--command", nixPrefetchCommand
                     ]
      nsStdin = ""
  (exitCode, nsStdout, nsStderr) <-
    readProcessWithExitCode "nix-shell" nixShellArgs nsStdin
  hPutStrLn stderr nsStderr
  pure $ case exitCode of
    ExitSuccess   ->
      case decode (fromString nsStdout) of
        Nothing -> Left (InvalidPrefetchGitOutput (pack nsStdout))
        Just o  -> Right o
    ExitFailure e -> Left (NixShellFailed e)

prefetchGitOutputToSpanUpdate :: FetchGitUpdateInfo
                              -> NixPrefetchGitOutput
                              -> FetchGitSpanUpdates
prefetchGitOutputToSpanUpdate u o =
  FetchGitSpanUpdates (SpanUpdate (revPos u) (quote $ rev o))
                      (SpanUpdate (sha256Pos u) (quote $ sha256 o))
  where quote t = "\"" <> t <> "\""

fetchGitUpdateInfos :: NExprLoc -> Either Warning [FetchGitUpdateInfo]
fetchGitUpdateInfos e = do
  ass <- fetchGitValues e
  traverse fetchGitArgsToUpdate ass

fetchGitArgsToUpdate :: FetchGitArgs -> Either Warning FetchGitUpdateInfo
fetchGitArgsToUpdate as = FetchGitUpdateInfo <$> exprText (urlStringExpr as)
                                             <*> exprSpan (revExpr as)
                                             <*> exprSpan (sha256Expr as)

fetchgitCalleeNames :: [Text]
fetchgitCalleeNames = ["fetchgit", "fetchgitPrivate"]

-- | Extract the calls to `fetchgit` in a nix expression
fetchGitValues :: NExprLoc -> Either Warning [FetchGitArgs]
fetchGitValues e =
  let exps = universe e
      fetchGitArgs = [a | AnnE _ (NApp (AnnE _ (NSym fg)) a) <- exps
                        , fg `elem` fetchgitCalleeNames
                        ]
  in traverse extractFetchGitArgs fetchGitArgs

extractFetchGitArgs :: NExprLoc -> Either Warning FetchGitArgs
extractFetchGitArgs = \case
  AnnE _ (NSet bindings) ->
    FetchGitArgs <$> extractAttr "url" bindings
                 <*> extractAttr "rev" bindings
                 <*> extractAttr "sha256" bindings
  e -> Left (ArgNotASet e)

