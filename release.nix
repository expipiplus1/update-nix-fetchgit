{ pkgs ? import ./nixpkgs.nix, compiler ? null }:

with pkgs.haskell.lib;

let drv = import ./default.nix { inherit pkgs compiler; };

in {
  tarball = sdistTarball drv;
  docs = documentationTarball drv;
  sdistTest = buildFromSdist drv;
}
