{
  src = builtins.fetchTarball {
    url = "file:///tmp/nix-update-fetchgit-test/archive.tar.gz";
    sha256 = "123";
  };
}
