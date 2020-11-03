{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/NixOS/nixpkgs/archive/50a8d606f3527b88fbd9f68048944470c1b900cd.tar.gz"; # haskell-updates
  sha256 = "07xlglmlhf3q6h8cdi3rfg4b0jk70h42kwpgwdh5b1vz149njins";
}, pkgs ? import nixpkgsSrc { }, compiler ? null, hoogle ? true
, forShell ? pkgs.lib.inNixShell }:

let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  compiler' = if compiler != null then
    compiler
  else
    "ghc" + pkgs.lib.concatStrings
    (pkgs.lib.splitVersion pkgs.haskellPackages.ghc.version);

  # Any overrides we require to the specified haskell package set
  haskellPackages = with pkgs.haskell.lib;
    pkgs.haskell.packages.${compiler'}.override {
      overrides = self: super:
        {
          data-fix = self.data-fix_0_3_0;
          optparse-generic = self.optparse-generic_1_4_4;
          optparse-applicative = self.optparse-applicative_0_16_0_0;
        } // pkgs.lib.optionalAttrs hoogle {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        };
    };

  # Any packages to appear in the environment provisioned by nix-shell
  extraPackages = [ pkgs.git pkgs.nix pkgs.nix-prefetch-git ];
  withExtras = drv:
    drv.overrideAttrs
    (attrs: { buildInputs = attrs.buildInputs ++ extraPackages; });

  # Generate a haskell derivation using the cabal2nix tool on `update-nix-fetchgit.cabal`
  drv = haskellPackages.callCabal2nix "update-nix-fetchgit" src { };

in withExtras (if forShell then drv.env else drv)
