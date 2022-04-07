let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/3bde7576132241b83f37d53d09bac5ba8458a1ee.tar.gz"; # haskell-updates
    sha256 = "0r83g6wc18cz5fp8dqw836r5aa4jdwlnd9ka4q44q0xzm5mlzw8z";
  };

in import nixpkgsSrc { }

