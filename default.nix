{ pkgs ? import ./nixpkgs.nix, compiler ? null }:

with pkgs;

let
  hp =
    if compiler == null then haskellPackages else haskell.packages.${compiler};

in hp.developPackage {
  name = "";
  root = nix-gitignore.gitignoreSource [ ] ./.;
  overrides = self: _super: {
    optparse-generic = self.optparse-generic_1_4_4;
    optparse-applicative = self.optparse-applicative_0_16_0_0;
  };
  modifier = drv: haskell.lib.addBuildTools drv [ git nix nix-prefetch-git ];
}

