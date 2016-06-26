# update-nix-fetchgit

## What this does:

- Parses a nix expression.
- Extracts the values which are a call to `fetchgit` or `fetchgitPrivate`.
- Runs `nix-prefetch-git` on the URLs from those calls.
- Uses the output of that `nix-prefetch-git` to get the latest commit hash and sha256 sum for the repo.
- Inserts the new commit hash and sha256 sum into the file.

## Under development

Please report any issues!

I'm `jophish` on Freenode; say hi!
