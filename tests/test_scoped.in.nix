{
  src = pkgs.fetchgit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "123";
    sha256 = "beef";
  };
  src = foo.bar.pkgs.fetchgit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "123";
    sha256 = "beef";
  };
}
