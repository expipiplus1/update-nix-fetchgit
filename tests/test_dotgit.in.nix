{ pkgs ? import <nixpkgs> { } }:

with pkgs;

{
  src = fetchgit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "123";
    sha256 = "beef";
  };

  srcDotGit = fetchgit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "123";
    sha256 = "beef";
    leaveDotGit = true;
  };

  srcDeep = fetchgit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "123";
    sha256 = "beef";
    deepClone = true;
  };
}
