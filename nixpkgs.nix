let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/83d907fd760d9ee4f49b4b7e4b1c6682f137b573.tar.gz"; # nixos-unstable
    sha256 = "0qc8gfkbga7v5p8r6h2hbdi8bjl3piikdwcbb9fff5cjnxi7v7qa";
  };

in import nixpkgsSrc { }

