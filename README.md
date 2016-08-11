# update-nix-fetchgit

This is a command-line utility for updating `fetchgit`, `fetchgitPrivate`, and `fetchFromGitHub` calls in [Nix](http://nixos.org/nix/) expressions.  This utility is meant to be used by people maintaining Nix expressions that fetch files from Git repositories.  It automates the process of keeping such expressions up-to-date with the latest upstream sources.

When you run `update-nix-fetchgit` on a file, it will:

- Read the file and parse it as a Nix expression.
- Find all Git fetches (calls to `fetchgit`, `fetchgitPrivate`, or `fetchFromGitHub`).
- Run [`nix-prefetch-git`](https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/fetchgit/nix-prefetch-git) to get information about the latest HEAD commit of each repository.
- Update the corresponding rev, sha256, and version attributes for each repository.
- Overwrite the original input file.

Any `version` attribute found in the file will be updated if it is in a set that contains (directly or inderictly) a Git fetch.  The version attribute will be updated to the commit date of the latest HEAD commit in the Git repository, in the time zone of the committer, in "YYYY-MM-DD" format.  If the set contains multiple Git fetches, the latest such date is used.

When this program fetches information from multiple repositories, it runs multiple instances of `nix-prefetch-git` in parallel.


# Usage

Pass the name of the file to be updated as the first argument:

    update-nix-fetchgit filename.nix

The file will be updated in place.


# Example

Here is an example of a Nix expression that can be updated by this program:

    { stdenv, fetchgit }:

    stdenv.mkDerivation rec {
      name = "foo-${version}";
      version = "2016-07-13";
      src = fetchgit {
        url = "git://midipix.org/slibtool";
        rev = "4f56fd184ef6020626492a6f954a486d54f8b7ba";
        sha256 = "0nmyp5yrzl9dbq85wyiimsj9fklb8637a1936nw7zzvlnzkgh28n";
      };
    }

The `rev`, `sha256`, and `version` attributes will all be updated.


# Building from source

The recommended way to build this program from source for development purposes is to download and run `nix-shell` in the top-level source directory and then run `cabal build`.


# More documentation

You can run `update-nix-fetchgit --help` or `man update-nix-fetchgit` for more documentation.


# Authors

- [expipiplus1](https://github.com/expipiplus1) - I'm `jophish` on Freenode; say hi!
- [DavidEGrayson](https://github.com/DavidEGrayson)