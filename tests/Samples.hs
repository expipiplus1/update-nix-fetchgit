{-# LANGUAGE OverloadedStrings #-}

module Samples where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Golden (goldenVsFile)
import           System.FilePath ((</>))

import qualified Data.Text
import qualified Data.Text.IO
import qualified System.Environment
import qualified System.FilePath
import qualified System.Directory
import qualified System.IO
import qualified System.IO.Temp
import qualified System.Process

import qualified Update.Nix.FetchGit
import           Update.Nix.FetchGit.Types (Env(Env))

-- | Provided output file @f@ pointing to e.g. @tests/test_rec_sets.out.nix@
-- * turn this into @tests/test_rec_sets.in.nix@
-- * copy to temporary directory
-- * create fake git repositories using @tests/fakeRepo.sh@
-- * adjust @url@ to point to the faked git repositories in the temporary directory
-- * perform update
-- * adjust @url@ to point to the expected one
-- * copy file back to @tests/test_rec_sets.out.nix@ so it be compared to @expected.nix@
runTest f =
  System.IO.Temp.withSystemTempDirectory "test-update-nix-fetchgit" $ \dir ->
    System.IO.Temp.withSystemTempDirectory "test-update-nix-fetchgit-store" $ \storeDir -> do
  let
      inFile = Data.Text.unpack
        $ Data.Text.replace ".out.nix" ".in.nix"
        $ Data.Text.pack f
      inBase = System.FilePath.takeBaseName inFile

  System.Directory.copyFile inFile (dir </> inBase)

  System.Directory.copyFile "tests/fakeRepo.sh" (dir </> "fakeRepo.sh")

  _ <- System.Process.readCreateProcess
        ((System.Process.shell (dir </> "fakeRepo.sh")) { System.Process.cwd = Just dir })
        mempty

  replaceFile (dir </> inBase) "/tmp/nix-update-fetchgit-test" (Data.Text.pack dir)

  System.Environment.setEnv "NIX_STATE_DIR" $ storeDir </> "state"
  System.Environment.setEnv "NIX_STORE_DIR" $ storeDir

  -- work around race condition https://github.com/NixOS/nix/issues/2706
  System.Directory.createDirectoryIfMissing True $ storeDir </> "state/gcroots"

  -- and another - error: SQLite database storeDir </> 'state/db/db.sqlite' is busy
  _ <- System.Process.readCreateProcess
        (System.Process.shell ("nix-store --init"))
        mempty

  let env = Env (const (Data.Text.IO.hPutStrLn System.IO.stderr)) []
  Update.Nix.FetchGit.processFile env (dir </> inBase)

  replaceFile (dir </> inBase) (Data.Text.pack dir) "/tmp/nix-update-fetchgit-test"

  System.Directory.copyFile (dir </> inBase) f
  where
    replaceFile f what with =
      Data.Text.IO.readFile f >>= Data.Text.IO.writeFile f . Data.Text.replace what with

allSamples :: [String]
allSamples =
  [ "test_builtins_fetchgit"
  , "test_builtins_fetchgit_update_ignores_sha256"
  , "test_builtins_fetchtarball_github"
  , "test_builtins_fetchtarball"
  , "test_dotgit"
  , "test_github_submodules"
  , "test_max_version"
  , "test_readme_examples"
  , "test_rec_sets"
  , "test_scoped"
  , "test_simple_update"
  , "test_updates_with_other_errors"
  , "test_version_update"
  ]

test_derivation :: TestTree
test_derivation = testGroup "golden" $ map mk allSamples
  where
    mk n =
      let
        fp   = "tests/"
        tEx  = (fp ++ n ++ ".expected.nix")
        tOut = (fp ++ n ++ ".out.nix")
      in
        goldenVsFile
          ("update of " ++ tOut)
          tEx tOut (runTest tOut)