let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/3561ed6953f96e2178142b04413d200be94e281d.tar.gz"; # master
    sha256 = "1lsg2dw4ifilywis3638023h4c8b073k2v56qzqqkhl7jkaiphbk";
  };

in import nixpkgsSrc { }

