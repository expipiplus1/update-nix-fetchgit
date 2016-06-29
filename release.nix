{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedPlatforms ? [ "x86_64-linux" "x86_64-darwin" ]
, supportedCompilers ? [ "ghc7103" ]
}:

with (import <nixpkgs> {}).lib;

let
  hnixSrc = (import <nixpkgs> {}).fetchFromGitHub{
    owner = "expipiplus1";
    repo = "hnix";
    rev = "a19a943f9b2b0f937c6fc6ce309bf425659133c9";
    sha256 = "0wxv5gmq7w3j8rzs000hskmbvdizcrhf44vpjw2xja519a4fz2r8";
  };

in

genAttrs supportedCompilers (ghcVer:
  genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };

      baseHaskellPackages = getAttrFromPath ["haskell" "packages" ghcVer] pkgs;

      haskellPackages = baseHaskellPackages.override {
        overrides = self: super: {
          hnix = self.callPackage hnixSrc {compiler = ghcVer; };
        };
      };
    in
      haskellPackages.callPackage src {}
  )
)
