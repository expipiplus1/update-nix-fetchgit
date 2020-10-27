# update-nix-fetchgit

This is a command-line utility for updating fetcher calls in
[Nix](http://nixos.org/nix/) expressions. It has two primary purposes:

- Automating the process of keeping such expressions up-to-date with the latest
  sources

- Filling hashes automatically instead of copying from the error message<sup>[1](#deny)</sup>.

The following fetchers are supported:

- `fetchgit`
- `fetchgitPrivate`
- `fetchFromGitHub`
- `fetchFromGitLab`
- `builtins.fetchGit`
- `builtins.fetchTarball`

The options `deepClone`, `leaveDotGit`, `fetchSubmodules` are also supported.

Additionally if the `rev` (for git fetches) or `url` attribute has a comment:

- `pin`: the revision or URL will not be updated, only the hash will be
  changed
- Anything else: the `rev` or `url` parameter will be updated to point to the
  revision pointed to by the branch or tag named in the comment.

## Usage

Pass the name of the files to be updated in place or pass no files to read and
write to `stdin` and `stdout` (useful as a filter in an editor).

`update-nix-fetchgit file1.nix file2.nix`

`update-nix-fetchgit <file1.nix >file1-updated.nix`

It will update fetchers anywhere in the files, note that it is a purely
syntactic match so complicated invocations of the fetchers may not be picked
up; see <./src/Update/Nix/Updater.hs> to look at the shapes of Nix expressions
which are matched.

Please open an issue if `update-nix-fetchgit` doesn't recognize a fetcher and
you think it could.

## Examples

Here are some examples of nix expressions which can be updated:

- Updating `src` and `version`

    ```nix
    { stdenv, fetchgit }:

    stdenv.mkDerivation rec {
      name = "foo-${version}";
      version = "2016-07-13";
      # ^ version will be updated to the date of the new revision
      src = fetchgit {
        url = "git://midipix.org/slibtool";
        rev = "4f56fd184ef6020626492a6f954a486d54f8b7ba";
        # ^ rev will be updated to the revision of HEAD
        sha256 = "0nmyp5yrzl9dbq85wyiimsj9fklb8637a1936nw7zzvlnzkgh28n";
        # ^ sha256 will be updated to the correct hash
      };
    }
    ```

- Following a branch fetched with `builtins.fetchTarball`

    ```nix
    { pkgs ? import (builtins.fetchTarball {
      url =
        "https://github.com/NixOS/nixpkgs/archive/foobar.tar.gz"; # nixos-unstable
        # ^ 'foobar' will be replaced with the revision pointed to by 'refs/heads/nixos-unstable'
      sha256 = "";
      # ^ sha256 will be updated to the correct hash
    }) { } }:

    myExpression
    ```

- Updating the hash (instead of trying to build and copying the hash from the
  error message)

    ```nix
    {
      upfind = import (pkgs.fetchFromGitHub {
        owner = "expipiplus1";
        repo = "upfind";
        rev = "cb451254f5b112f839aa36e5b6fd83b60cf9b9ae"; # pin
        # ^ This will not change because of the '# pin' comment
        sha256 = _;
        # ^ This will be updated
      }) { };
    }
    ```

## Mechanism

When you run `update-nix-fetchgit` on a file, it will:

- Read the file and parse it as a Nix expression.
- Find all calls to fetchers.
- Run
  [`nix-prefetch-git`](https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/fetchgit/nix-prefetch-git)
  or `nix-prefetch-url` (and a call to the GitHub API to get the commit date in
  the case of `builtins.fetchTarball`) to get information about the latest
  version.
- Update the corresponding `rev`, `sha256`, `url`, and `version` attributes for
  each repository.
- Overwrite the original input file or print to `stdout`

Any `version` attribute found in the file will be updated if it is in a set
that contains (directly or indirectly) a Git fetch. The version attribute will
be updated to the commit date of the latest HEAD commit in the Git repository,
in the time zone of the committer, in "YYYY-MM-DD" format. If the set contains
multiple Git fetches, the latest such date is used.

## Building from source

The recommended way to build this program from source for development purposes
is to download and run `nix-shell` in the top-level source directory and then
run `cabal build`.

# Authors

- [expipiplus1](https://github.com/expipiplus1) - I'm `jophish` on Freenode; say hi!
- [DavidEGrayson](https://github.com/DavidEGrayson)

--------

<a name="deny">1</a>: Don't deny you do it
