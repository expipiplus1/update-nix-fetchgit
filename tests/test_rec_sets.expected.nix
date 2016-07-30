rec {
  name = "foobar-${version}";
  version = "2016-06-26";
  src = fetchgit rec {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "1c60ae07b5740aab02e32b4f64600f002112e6fd";
    sha256 = "008xwkjfsv3rj50x9fqj0vvggl1zwrbqh7bkvnga94krmij63hib";
  };
}
