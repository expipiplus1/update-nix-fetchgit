{
  src = builtins.fetchGit {
    url = "/tmp/nix-update-fetchgit-test/repo1";
    rev = "1c60ae07b5740aab02e32b4f64600f002112e6fd";
  };
}
