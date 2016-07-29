{
  version = "2016-01-01";
  repos = {
    version = "2016-01-01";
    repo1 = {
      version = "2016-01-01";
      src = fetchgit {
        url = "/tmp/nix-update-fetchgit-test/repo1";
        rev = "123";
        sha256 = "beef";
      };
    };
    repo2 = {
      version = "2016-01-01";
      src = fetchgit {
        url = "/tmp/nix-update-fetchgit-test/repo2";
        rev = "456";
        sha256 = "feeb";
      };
    };
  };
}
