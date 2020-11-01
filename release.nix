{ pkgs ? import <nixpkgs> { }, compiler ? "ghc884" }:

with pkgs.haskell.lib;

let
  drv = import ./default.nix {
    inherit pkgs compiler;
    hoogle = false;
    forShell = false;
  };

  docDrv = drv:
    (overrideCabal drv (drv: {
      doHaddock = true;
      haddockFlags = [ "--for-hackage" ];
      postHaddock = ''
        mkdir -p "$doc"
        tar --format=ustar -czf "$doc/${drv.pname}-${drv.version}-docs.tar.gz" -C dist/doc/html "${drv.pname}-${drv.version}-docs"
      '';
    })).doc;

in {
  tarball = sdistTarball drv;
  docs = docDrv drv;
  sdistTest = buildFromSdist drv;
}
