let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/c6c4a3d45ab200f17805d2d86a1ff1cc7ca2b186.tar.gz"; # nixos-unstable
    sha256 = "1f6q98vx3sqxcn6qp5vpy00223r9hy93w9pxq65h9gdwzy3w4qxv";
  };

in import nixpkgsSrc { }

