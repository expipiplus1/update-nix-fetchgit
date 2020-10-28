{
  # Pinning a specific revision
  # Hash will be updated
  src1 = builtins.fetchTarball {
    url =
      "https://github.com/expipiplus1/update-nix-fetchgit/archive/4ac47efd681f530baca71f1d27d43c50ba19bb72.tar.gz"; # pin
    sha256 = "1pr0r2vc0fnpk3fm17d4gnd659nkqnih1ik3bqarr3lykgg3pci6";
  };

  # Updating URL to use a tag
  # Hash will be updated
  src2 = builtins.fetchTarball {
    url =
      "https://github.com/expipiplus1/update-nix-fetchgit/archive/0.1.0.0.tar.gz"; # 0.1.0.0
    sha256 = "0zhng69b6lr8dbdwrw09glbyavw7cfqvm3gb4xqxx973iajifmv7";
  };
}
