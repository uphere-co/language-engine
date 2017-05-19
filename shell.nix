{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let 
    newHaskellPackages = haskellPackages;
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              attoparsec
              cabal-install
              haskeline
              lens
              monad-loops
              optparse-applicative
              split
              text text-format 
            ]);
in stdenv.mkDerivation {
  name = "textview-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
