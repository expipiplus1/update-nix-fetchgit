{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
}:

let
  pkgs = import <nixpkgs> { };

  hnixSrc = pkgs.fetchFromGitHub {
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

  # TODO: Use pkgs.haskellPackages after we fix the build on GHC 8.
  # We would need to fix https://github.com/jwiegley/hnix/issues/39
  baseHaskellPackages = pkgs.haskell.packages.ghc7103;

  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: {
      vector-algorithms = addBuildDepends super.vector-algorithms
                          (with self; [mtl mwc-random]);
      hnix = self.callPackage (hnixSrc + "/project.nix") { };
    };
  };
in
  haskellPackages.callPackage src {}
