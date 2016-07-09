{
  version = "2016-07-08";
  repos = {
    version = "2016-07-08";
    repo1 = {
      version = "2016-06-26";
      src = fetchgit {
        url = "/tmp/nix-update-fetchgit-test/repo1";
        rev = "1c60ae07b5740aab02e32b4f64600f002112e6fd";
        sha256 = "008xwkjfsv3rj50x9fqj0vvggl1zwrbqh7bkvnga94krmij63hib";
      };
    };
    repo2 = {
      version = "2016-07-08";
      src = fetchgit {
        url = "/tmp/nix-update-fetchgit-test/repo2";
        rev = "1ac598e1fd0ec3556f1349bb5fd8d08d89580c8a";
        sha256 = "0fbv29f0qi6kjvjd9ancw80qhvj4ylv8jmr726mj001q4vacjs18";
      };
    };
  };
}
