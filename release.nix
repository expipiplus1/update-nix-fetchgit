{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
}:

let
  pkgs = import <nixpkgs> { };

  hnixSrc = pkgs.fetchFromGitHub{
    owner = "jwiegley";
    repo = "hnix";
    rev = "e2b80391bb731c80995a3a7f2dc0df6685c643c6";
    sha256 = "0wxv5gmq7w3j8rzs000hskmbvdizcrhf44vpjw2xja519a4fz2r8";
  };

  baseHaskellPackages = pkgs.haskellPackages;

  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: {
      hnix = self.callPackage (hnixSrc + "/project.nix") { };
    };
  };
in
  haskellPackages.callPackage src {}
