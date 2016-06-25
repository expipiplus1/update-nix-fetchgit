{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedPlatforms ? [ "x86_64-linux" ]
, supportedCompilers ? [ "ghc7103" ]
}:

with (import <nixpkgs> {}).lib;

genAttrs supportedCompilers (ghcVer:
  genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };

      haskellPackages = getAttrFromPath ["haskell" "packages" ghcVer] pkgs;
    in
      haskellPackages.callPackage src {}
  )
)
