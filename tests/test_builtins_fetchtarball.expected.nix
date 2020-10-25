{
  src = builtins.fetchTarball {
    url = "file:///tmp/nix-update-fetchgit-test/archive.tar.gz";
    sha256 = "1xmr8jicvzszfzpz46g37mlpvbzjl2wpwvl2b05psipssyp1sm8h";
  };
}
