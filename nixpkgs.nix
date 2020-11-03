let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/50a8d606f3527b88fbd9f68048944470c1b900cd.tar.gz"; # haskell-updates
    sha256 = "07xlglmlhf3q6h8cdi3rfg4b0jk70h42kwpgwdh5b1vz149njins";
  };

in import nixpkgsSrc { }

