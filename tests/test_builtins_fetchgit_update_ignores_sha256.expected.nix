{
  src = builtins.fetchGit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "1c60ae07b5740aab02e32b4f64600f002112e6fd";
    # this is an invalid definition since fetchGit doesn't take a sha256
    # parameter, but the point of this test is to make sure the fields gets
    # ignored regardless.
    sha256 = "IGNORED";
  };
}
