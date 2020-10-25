#!/usr/bin/env bash

# Bash script for testing update-nix-fetchgit.
#
# Unlike git, nix-prefetch-git does not work with git URLs that are
# relative paths, so we store local test repositories at fixed
# locations inside /tmp/nix-update-fetchgit-test.  We use file locking
# to make sure that multiple instances of this script can be run at
# the same time without stepping on eachother.

set -ue

# Change to the directory where this script is located.
cd $(dirname $0)

updater="$( echo ../dist-newstyle/build/*/*/update-nix-fetchgit-*/x/update-nix-fetchgit/opt/build/update-nix-fetchgit/update-nix-fetchgit )"

# Prepares git repositories on the local machine that we will use for
# testing.
function prepare_local_test_assets() {
  # Make sure the commit hashes are predictable.
  export GIT_AUTHOR_DATE='1466974421 +0200'
  export GIT_COMMITTER_DATE='1466974421 +0200'
  export GIT_COMMITTER_NAME='joe'
  export GIT_AUTHOR_NAME='joe'
  export GIT_COMMITTER_EMAIL='joe@example.com'
  export GIT_AUTHOR_EMAIL='joe@example.com'

  rm -rf /tmp/nix-update-fetchgit-test/
  mkdir -p /tmp/nix-update-fetchgit-test/
  pushd /tmp/nix-update-fetchgit-test/

  git init repo1
  pushd repo1
  echo hi > test.txt
  git add test.txt
  git commit -m "initial commit"
  echo 1.0.0 > test.txt
  git commit -m "version 1.0.0" test.txt
  git tag 1.0.0
  echo '1.0.0+stuff' > test.txt
  git commit -m "added stuff" test.txt
  popd

  export GIT_AUTHOR_DATE='1468031426 -0700'
  export GIT_COMMITTER_DATE='1468031426 -0700'
  git init repo2
  pushd repo2
  echo hi > test.txt
  git add test.txt
  git commit -m "initial commit"
  popd

  echo hi > test.txt
  tar czvf archive.tar.gz test.txt

  popd
}

function error {
  echo "$1" >&2
  exit 1
}

function test_successful_update() {
  local test_name="$1"
  cp $test_name.in.nix $test_name.out.nix
  echo "$test_name: Starting."
  if ! "$updater" $test_name.out.nix; then
      error "$test_name: Error running the updater."
  fi
  if ! diff $test_name.expected.nix $test_name.out.nix; then
      error "$test_name: Incorrect output."
  fi
  echo "$test_name: Passed."
}

function run_test_suite() {
  prepare_local_test_assets > /dev/null

  for f in *.expected.nix; do
    local test_name=$(basename $f .expected.nix)
    test_successful_update $test_name
  done

  echo
  echo "All tests passed."
}

{
  # Acquire an exclusive lock on file descriptor 200, but time out
  # after 10 minutes if it cannot be acquired.
  flock -x -w 600 200

  run_test_suite

} 200> /tmp/nix-update-fetchgit-test-lock
