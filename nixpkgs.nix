let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/d42cd445dde587e9a993cd9434cb43da07c4c5de.tar.gz"; # nixos-unstable
    sha256 = "0dzrn97srxyw5a3g7hf8chwccxns5z3aij23hc0fch7ygc8w0gq0";
  };

in import nixpkgsSrc { }

