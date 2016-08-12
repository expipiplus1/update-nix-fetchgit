{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
}:

let
  # The last commit of nixpkgs known to work when imported here is
  # c66ca712b9c4af60477b7582f73a14ab975ffda7 (2016-08-11).
  pkgs = import <nixpkgs> { };

  hnixSrc = pkgs.fetchFromGitHub {
    owner = "jwiegley";
    repo = "hnix";
    rev = "e2b80391bb731c80995a3a7f2dc0df6685c643c6";
    sha256 = "0wxv5gmq7w3j8rzs000hskmbvdizcrhf44vpjw2xja519a4fz2r8";
  };

  # TODO: Use pkgs.haskellPackages after we fix the build on GHC 8.
  # We would need to fix https://github.com/jwiegley/hnix/issues/39
  baseHaskellPackages = pkgs.haskell.packages.ghc7103;

  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: {
      hnix = self.callPackage (hnixSrc + "/project.nix") { };
    };
  };
in
  haskellPackages.callPackage src {}
