{ pkgs ? import <nixpkgs> { }, compiler ? null, hoogle ? true }:

(import ./default.nix { inherit pkgs compiler hoogle; }).env
