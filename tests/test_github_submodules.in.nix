{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  regular = fetchFromGitHub {
    owner = "expipiplus1";
    repo = "has-submodule";
    rev = "a-tag"; # a-tag
    sha256 = "";
  };

  submodules = fetchFromGitHub {
    owner = "expipiplus1";
    repo = "has-submodule";
    rev = "a-tag"; # a-tag
    sha256 = "";
    fetchSubmodules = true;
  };

in { inherit regular submodules;}
