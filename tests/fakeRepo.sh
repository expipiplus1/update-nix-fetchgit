#!/bin/sh

# Prepares git repositories on the local machine that we will use for
# testing.

set -ex

# Make sure the commit hashes are predictable.
export GIT_AUTHOR_DATE='1466974421 +0200'
export GIT_COMMITTER_DATE='1466974421 +0200'
export GIT_COMMITTER_NAME='joe'
export GIT_AUTHOR_NAME='joe'
export GIT_COMMITTER_EMAIL='joe@example.com'
export GIT_AUTHOR_EMAIL='joe@example.com'

git init repo1
cd repo1
  echo hi > test.txt
  git add test.txt
  git commit -m "initial commit"
  echo 1.0.0 > test.txt
  git commit -m "version 1.0.0" test.txt
  git tag 1.0.0
  echo '1.0.0+stuff' > test.txt
  git commit -m "added stuff" test.txt
cd ..

export GIT_AUTHOR_DATE='1468031426 -0700'
export GIT_COMMITTER_DATE='1468031426 -0700'
git init repo2
cd repo2
  echo hi > test.txt
  git add test.txt
  git commit -m "initial commit"
cd ..
