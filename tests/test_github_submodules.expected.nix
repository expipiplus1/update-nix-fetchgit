{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  regular = fetchFromGitHub {
    owner = "expipiplus1";
    repo = "has-submodule";
    rev = "101c71e91ec95e0dcd724b0122cf55e52aee7361"; # a-tag
    sha256 = "0km9m9krsi0pmx10qy1s5z0w4dp4h0v9xj2fgm255bdpdhm4rq55";
  };

  submodules = fetchFromGitHub {
    owner = "expipiplus1";
    repo = "has-submodule";
    rev = "101c71e91ec95e0dcd724b0122cf55e52aee7361"; # a-tag
    sha256 = "1w386008njxfwnwm4xrb3fip5wmm3nwhrfl4mb96ahhj0mcg493g";
    fetchSubmodules = true;
  };

in { inherit regular submodules;}
