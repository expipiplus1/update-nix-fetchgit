{
  src = builtins.fetchGit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "123";
  };
}
