{ mkDerivation, aeson, ansi-wl-pprint, async, base, bytestring
, data-fix, errors, hnix, process, stdenv, text, time, transformers
, uniplate, utf8-string
}:
mkDerivation {
  pname = "update-nix-fetchgit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint async base bytestring data-fix errors hnix
    process text time transformers uniplate utf8-string
  ];
  executableHaskellDepends = [ base text ];
  homepage = "https://github.com/expipiplus1/update-nix-fetchgit#readme";
  description = "A program to update fetchgit values in Nix expressions";
  license = stdenv.lib.licenses.bsd3;
}
