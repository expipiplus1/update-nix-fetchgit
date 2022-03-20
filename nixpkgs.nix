let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/16f3fbbd6f332daeb36c1c3fa75587ff6d6faf5a.tar.gz"; # haskell-updates
    sha256 = "1c1f0ppqidffbgnqc008ljazqmb3wisd92g6w4xq31bq98payxnf";
  };

in import nixpkgsSrc { }

