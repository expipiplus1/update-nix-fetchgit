{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedPlatforms ? [ "x86_64-linux" "x86_64-darwin" ]
, supportedCompilers ? [ "ghc7103" ]
}:

let
  nixpkgsSrc = (import <nixpkgs> {}).fetchFromGitHub{
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "199f5a2844cb31be96872599d8db7105b572f116";
    sha256 = "0q4d1fxi98m4mial192247clhpk4y149zwgd2vk6gz9710vyxdck";
  };

  pkgs = import nixpkgsSrc {};

  hnixSrc = pkgs.fetchFromGitHub{
    owner = "jwiegley";
    repo = "hnix";
    rev = "e2b80391bb731c80995a3a7f2dc0df6685c643c6";
    sha256 = "0wxv5gmq7w3j8rzs000hskmbvdizcrhf44vpjw2xja519a4fz2r8";
  };

  addBuildDepends = package: newDepends: package.override (args: args // {
    mkDerivation = expr: args.mkDerivation (expr // {
      buildDepends = (expr.buildDepends or []) ++ newDepends;
    });
  });

in

with pkgs.lib;

genAttrs supportedCompilers (ghcVer:
  genAttrs supportedPlatforms (system:
    let
      pkgs = import nixpkgsSrc { inherit system; };

      baseHaskellPackages = getAttrFromPath ["haskell" "packages" ghcVer] pkgs;

      haskellPackages = baseHaskellPackages.override {
        overrides = self: super: {
          vector-algorithms = addBuildDepends super.vector-algorithms
                                          (with self; [mtl mwc-random]);

          hnix = self.callPackage (hnixSrc + "/project.nix") {};
        };
      };
    in
      haskellPackages.callPackage src {}
  )
)
