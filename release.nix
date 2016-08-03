{ src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedPlatforms ? [ "x86_64-linux" "x86_64-darwin" ]
, supportedCompilers ? [ "ghc7103" ]
}:

let
  pkgs = import ((import <nixpkgs> {}).fetchFromGitHub{
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "49ad8ce561d2c6ae45a178cb431b87ca2b07fb8b";
    sha256 = "1n2md7xif8q0qbzh8766sfc4mi4hyhk04rqp7q3gfvki7nyjkjw5";
  }) {};

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
      pkgs = import <nixpkgs> { inherit system; };

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
