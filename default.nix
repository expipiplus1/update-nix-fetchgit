{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

# Strip out the irrelevant parts of the source
let src = with pkgs.lib;
          let p = n: t: toString ./dist != n && t != "unknown";
          in cleanSourceWith {filter = p; src = cleanSource ./.;};

    haskellPackages = pkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
      };
    };

    extraEnvPackages = [
    ];

    drv =
      haskellPackages.callCabal2nix "update-nix-fetchgit" src {};

    envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
      buildInputs = attrs.buildInputs ++ extraEnvPackages;
    });

in
  drv // { env = envWithExtras; }
