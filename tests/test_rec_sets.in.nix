rec {
  name = "foobar-${version}";
  version = "2016-01-01";
  src = fetchgit rec {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "123";
    sha256 = "beef";
  };
}
