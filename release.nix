{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedPlatforms ? [ "x86_64-linux" "x86_64-darwin" ]
, supportedCompilers ? [ "ghc7103" ]
}:

with (import <nixpkgs> {}).lib;

let
  hnixSrc = (import <nixpkgs> {}).fetchFromGitHub{
    owner = "expipiplus1";
    repo = "hnix";
    rev = "295e26b2081552d3a70e5a249dc61481e7482477";
    sha256 = "01h2vnkwc1izp7nw4j59nl07jnd3s6nrwlmr4ilkkngkxnrcl5vk";
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
