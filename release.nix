{ pkgs ? import ./nixpkgs.nix, compiler ? null }:

with pkgs.haskell.lib;

let drv = import ./default.nix { inherit pkgs compiler; forShell = false; };

in {
  tarball = sdistTarball drv;
  docs = documentationTarball drv;
  sdistTest = buildFromSdist drv;
}
