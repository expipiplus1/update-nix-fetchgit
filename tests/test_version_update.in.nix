{
  version = "2016-01-01";
  src = fetchgit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "123";
    sha256 = "beef";
  };
}
