{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let

  hsenv = haskellPackages.ghcWithPackages (p: with p; [
            attoparsec
            haskeline
            lens
            monad-loops
            optparse-applicative
            split
            text text-format
            #yayaml

            lens
            taggy-lens
          ]);

in

stdenv.mkDerivation {
  name = "PropBank-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

