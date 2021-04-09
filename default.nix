{ pkgs ? import ./nixpkgs.nix, compiler ? null }:

with pkgs;

let
  hp =
    if compiler == null then haskellPackages else haskell.packages.${compiler};

in hp.developPackage {
  name = "update-nix-fetchgit";
  root = nix-gitignore.gitignoreSource [ ] ./.;
  overrides = self: _super: {
  };
  modifier = drv: haskell.lib.addBuildTools drv [ git nix nix-prefetch-git ];
}

