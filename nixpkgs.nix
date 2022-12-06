let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/d7af8ae032b081ab01461be06838ba5ea89110f1.tar.gz"; # haskell-updates
    sha256 = "1n6ky9lczwh2dk2q0ckdv4wm096q1syk2pddrhidj1vy34aigpxd";
  };

in import nixpkgsSrc { }

