let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/967d40bec14be87262b21ab901dbace23b7365db.tar.gz"; # nixos-unstable
    sha256 = "0vkcqwkrz6csrrv7zk4v9cryv5jy75px4qnzw8h3zljpvwrdpra6";
  };

in import nixpkgsSrc { }

