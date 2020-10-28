{
  version = _;

  should-error = fetchgit rec {
    url = "/tmp/nix-update-fetchgit-test/missingRepo";
    rev = "123";
    sha256 = "beef";
  };

  should-update = fetchgit rec {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "123";
    sha256 = "beef";
  };
}
