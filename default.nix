{ pkgs ? import ./nixpkgs.nix, compiler ? null, forShell ? pkgs.lib.inNixShell
}:

with pkgs;

let
  hp =
    if compiler == null then haskellPackages else haskell.packages.${compiler};

in hp.developPackage {
  name = "update-nix-fetchgit";
  root = nix-gitignore.gitignoreSource [ ] ./.;
  overrides = self: super: {
    hnix = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
      pkg = "hnix";
      ver = "0.13.1";
      sha256 = "0v3r33azlv050fv8y5vw0pahdnch7vqq94viwrp9vlw8hpiys8qn";
    } { });
    relude = self.relude_1_0_0_1;
    semialign = self.semialign_1_2;
  };
  modifier = drv: haskell.lib.addBuildTools drv [ git nix nix-prefetch-git ];
  returnShellEnv = forShell;
}

