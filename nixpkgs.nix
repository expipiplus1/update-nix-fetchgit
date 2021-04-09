let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/4795e7f3a9cebe277bb4b5920caa8f0a2c313eb0.tar.gz"; # master
    sha256 = "03xqwcli1cw96x5d21siq9ph9g37igp35fv9rgsl3wv4gvm8y48c";
  };

in import nixpkgsSrc { }

