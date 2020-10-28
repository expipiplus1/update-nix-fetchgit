{
  a = { stdenv, fetchgit }:
    stdenv.mkDerivation rec {
      name = "foo-${version}";
      version = "2016-07-13";
      # ^ version will be updated to the date of the new revision
      src = fetchgit {
        url = "/tmp/nix-update-fetchgit-test/repo1";
        rev = "4f56fd184ef6020626492a6f954a486d54f8b7ba";
        # ^ rev will be updated to the revision of HEAD
        sha256 = "0nmyp5yrzl9dbq85wyiimsj9fklb8637a1936nw7zzvlnzkgh28n";
        # ^ sha256 will be updated to the correct hash
      };
    };

  b = { pkgs ? import (builtins.fetchTarball {
    url =
      "https://github.com/expipiplus1/update-nix-fetchgit/archive/foobar.tar.gz"; # 0.1.0.0
    # ^ 'foobar' will be replaced with the revision pointed to by 'refs/tags/0.1.0.0'
    sha256 = "";
    # ^ sha256 will be updated to the correct hash
  }) { } }:
    myExpression;

  c = {
    upfind = import (pkgs.fetchFromGitHub {
      owner = "expipiplus1";
      repo = "upfind";
      rev = "cb451254f5b112f839aa36e5b6fd83b60cf9b9ae"; # pin
      # ^ This will not change because of the '# pin' comment
      sha256 = _;
      # ^ This will be updated
    }) { };
  };
}
