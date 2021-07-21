{ pkgs ? import <nixpkgs> { } }:

with pkgs;

{
  src = fetchgit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "1c60ae07b5740aab02e32b4f64600f002112e6fd";
    sha256 = "008xwkjfsv3rj50x9fqj0vvggl1zwrbqh7bkvnga94krmij63hib";
  };

  srcDotGit = fetchgit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "1c60ae07b5740aab02e32b4f64600f002112e6fd";
    sha256 = "1ywf4ysn3fcr4dw24idz48i36ghvxbvi72icglp4nqhdj2s6bzj1";
    leaveDotGit = true;
  };

  srcDeep = fetchgit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "1c60ae07b5740aab02e32b4f64600f002112e6fd";
    sha256 = "1ywf4ysn3fcr4dw24idz48i36ghvxbvi72icglp4nqhdj2s6bzj1";
    deepClone = true;
  };
}
