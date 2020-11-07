{ pkgs ? import ./nixpkgs.nix, compiler ? "ghc884" }:

with pkgs.haskell.lib;

let
  drv = import ./default.nix { inherit pkgs compiler; };

  docDrv = pkg:
    pkgs.lib.overrideDerivation pkg (drv: {
      name = "${drv.name}-docs";
      outputs = [ "out" ];
      buildPhase = ''
        runHook preHaddock
        ./Setup haddock --for-hackage
        runHook postHaddock
      '';
      checkPhase = ":";
      installPhase = ''
        runHook preInstall
        mkdir -p "$out"
        tar --format=ustar \
          -czf "$out/${drv.pname}-${drv.version}-docs.tar.gz" \
          -C dist/doc/html "${drv.pname}-${drv.version}-docs"
        runHook postInstall
      '';
    });

in {
  tarball = sdistTarball drv;
  docs = docDrv drv;
  sdistTest = buildFromSdist drv;
}
