# Version [0.1.2.0](https://github.com/expipiplus1/update-nix-fetchgit/compare/0.1.1.0...0.1.2.0)

* Additions
  * Support updating `fetchFromGitLab`
  * Support updating `fetchgit` and `fetchgitPrivate

# Version [0.1.1.0](https://github.com/expipiplus1/update-nix-fetchgit/compare/0.1.0.0...0.1.1.0)

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

