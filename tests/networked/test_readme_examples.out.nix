{
  a = { stdenv, fetchgit }:
    stdenv.mkDerivation rec {
      name = "foo-${version}";
      version = "unstable-2016-06-26";
      # ^ version will be updated to the date of the new revision
      src = fetchgit {
        url = "/tmp/nix-update-fetchgit-test/repo1";
        rev = "1c60ae07b5740aab02e32b4f64600f002112e6fd";
        # ^ rev will be updated to the revision of HEAD
        sha256 = "008xwkjfsv3rj50x9fqj0vvggl1zwrbqh7bkvnga94krmij63hib";
        # ^ sha256 will be updated to the correct hash
      };
    };

  b = { pkgs ? import (builtins.fetchTarball {
    url =
      "https://github.com/expipiplus1/update-nix-fetchgit/archive/0.1.0.0.tar.gz"; # 0.1.0.0
    # ^ 'foobar' will be replaced with the revision pointed to by 'refs/tags/0.1.0.0'
    sha256 = "0zhng69b6lr8dbdwrw09glbyavw7cfqvm3gb4xqxx973iajifmv7";
    # ^ sha256 will be updated to the correct hash
  }) { } }:
    myExpression;

  c = {
    upfind = import (pkgs.fetchFromGitHub {
      owner = "expipiplus1";
      repo = "upfind";
      rev = "cb451254f5b112f839aa36e5b6fd83b60cf9b9ae"; # pin
      # ^ This will not change because of the '# pin' comment
      sha256 = "15g5nvs6azgb2fkdna1dxbyiabx9n63if0wcbdvs91hjafhzjaqa";
      # ^ This will be updated
    }) { };
  };
}
