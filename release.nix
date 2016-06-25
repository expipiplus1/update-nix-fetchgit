{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedPlatforms ? [ "x86_64-linux" ]
, supportedCompilers ? [ "ghc7103" ]
}:

with (import <nixpkgs> {}).lib;

genAttrs supportedCompilers (ghcVer:
  genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };

      baseHaskellPackages = getAttrFromPath ["haskell" "packages" ghcVer] pkgs;

      haskellPackages = baseHaskellPackages.override {
        overrides = self: super: {
          hnix = self.callPackage /home/jophish/src/hnix {compiler = ghcVer; };
        };
      };
    in
      haskellPackages.callPackage src {}
  )
)
