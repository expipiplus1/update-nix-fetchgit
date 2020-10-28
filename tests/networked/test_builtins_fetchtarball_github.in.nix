{
  # Pinning a specific revision
  # Hash will be updated
  src1 = builtins.fetchTarball {
    url =
      "https://github.com/expipiplus1/update-nix-fetchgit/archive/4ac47efd681f530baca71f1d27d43c50ba19bb72.tar.gz"; # pin
    sha256 = "123";
  };

  # Updating URL to use a tag
  # Hash will be updated
  src2 = builtins.fetchTarball {
    url =
      "https://github.com/expipiplus1/update-nix-fetchgit/archive/gone.tar.gz"; # 0.1.0.0
    sha256 = "123";
  };
}
