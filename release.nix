{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
}:

let
  # The last commit of nixpkgs known to work when imported here is
  # a9f054c8348bf8cd7630cd99def2fd86d22c78bd (2017-05-04).
  pkgs = import <nixpkgs> { };
in
  pkgs.haskell.packages.ghc802.callPackage src {}
