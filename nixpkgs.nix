let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/50d354db00061ebfc6992b6040e81e7bfbedd473.tar.gz"; # haskell-updates
    sha256 = "08i444lcs8m8kby1hcylbddlllyyh3dnxvy6lv0sicbymra4jw7z";
  };

in import nixpkgsSrc { }

