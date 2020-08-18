{
  src = builtins.fetchGit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "123";
    # this is an invalid definition since fetchGit doesn't take a sha256
    # parameter, but the point of this test is to make sure the fields gets
    # ignored regardless.
    sha256 = "IGNORED";
  };
}
