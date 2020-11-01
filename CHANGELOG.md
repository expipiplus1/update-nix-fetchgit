# Changelog

## WIP

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

