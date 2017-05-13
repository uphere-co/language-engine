{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let

  hsenv = haskellPackages.ghcWithPackages (p: with p; [
            lens
            taggy-lens
          ]);

in

stdenv.mkDerivation {
  name = "NomBank-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

