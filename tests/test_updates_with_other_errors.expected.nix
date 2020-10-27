{
  version = "2016-06-26";

  should-error = fetchgit rec {
    url = "/tmp/nix-update-fetchgit-test/missingRepo";
    rev = "123";
    sha256 = "beef";
  };

  should-update = fetchgit rec {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "1c60ae07b5740aab02e32b4f64600f002112e6fd";
    sha256 = "008xwkjfsv3rj50x9fqj0vvggl1zwrbqh7bkvnga94krmij63hib";
  };
}
