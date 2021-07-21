# Changelog

## WIP
- Fix dotgit / deepClone test sha256

## [0.2.8] - 2021-07-19
- Add `unstable-` prefix to versions when using dates

## [0.2.7] - 2021-05-23

- `hnix-0.13` support

## [0.2.6] - 2021-05-17

- `git ls-remote` calls are printed in verbose mode
- Small packaging changes

## [0.2.5] - 2020-11-14

- Pass `--heads --tags` to `git ls-remote` to avoid fetching remote
  refs. Don't pass when the revision begins with `refs`

## [0.2.4] - 2020-11-11

- Add --dry-run option
- Sort tags with git ls-remote with `-v:refname` (according to version)
  - this allows one to have a comment like `# tags/v*` to get the latest
    version
- Add --only-commented option to allow being a bit more explicit about what's
  updated

## [0.2.3] - 2020-11-06

- Implement filtering updates based on binding name
- Better error message output on parse failure
- Drop support for GHC 8.6

## [0.2.2] - 2020-11-03

- Require hnix version 0.11 with several important bugfixes

## [0.2.1] - 2020-11-01

- Add support for `callHackageDirect`

## [0.2] - 2020-10-28

- Update hashes in calls to `builtins.fetchTarball`
- Allow updating tarball urls in fetched from GitHub
- Read comments for branch/tag information
- Allow pinning revisions in tarball fetches
- Gate stderr output under `--verbose`
- Add support for `fetchSubmodules`, `leaveDotGit` and `deepClone`
- Add support for filtering update locations with `--location line:col`

### Under the Hood

- Use new unification method for Nix expressions, see ./src/Nix/Match.hs
- Rewrite matchers, now in ./src/Update/Nix/Updater.hs
- Use `monad-validate` for error handling

## Version [0.1.2.0](https://github.com/expipiplus1/update-nix-fetchgit/compare/0.1.1.0...0.1.2.0)

* Additions
  * Support updating `fetchFromGitLab`
  * Support updating `fetchgit` and `fetchgitPrivate

## Version [0.1.1.0](https://github.com/expipiplus1/update-nix-fetchgit/compare/0.1.0.0...0.1.1.0)

* Changelog started. Previous release was `0.1.0.0`.

* Additions
  * `update-nix-fetchgit` will pass any extra arguments after the filename to `nix-prefetch-git`:

  ```
  update-nix-fetchgit filename.nix --rev refs/heads/myBranch
  ```

  * `Update.Span` module now exposes `split`

---

`update-nix-fetchgit` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org

