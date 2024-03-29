{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
}:

with pkgs;

let
  hsconfig = import (uphere-nix-overlay+"/nix/haskell-modules/configuration-ghc-8.0.x.nix")
               { inherit pkgs; };
  newHaskellPackages = haskellPackages.override {
    overrides = hsconfig;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            aeson
            attoparsec
            bifunctors
            discrimination
            hashable
            lens
            monad-loops
            pretty-hex
            taggy-lens
            text
            cabal-install
          ]);

in

stdenv.mkDerivation {
  name = "nlp-types-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

