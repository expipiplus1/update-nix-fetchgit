{ mkDerivation, aeson, ansi-wl-pprint, async, base, bytestring
, data-fix, hnix, process, stdenv, text, transformers, uniplate
, utf8-string
}:
mkDerivation {
  pname = "update-nix-fetchgit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring data-fix hnix process text transformers
    uniplate utf8-string
  ];
  executableHaskellDepends = [ ansi-wl-pprint base hnix text ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/expipiplus1/update-nix-fetchgit#readme";
  description = "A program to update fetchgit values in Nix expressions";
  license = stdenv.lib.licenses.bsd3;
}
