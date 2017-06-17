{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
}:

with pkgs;

let
  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
               { inherit pkgs; };
  newHaskellPackages = haskellPackages.override {
    overrides = hsconfig;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            cabal-install
          ]);

in

stdenv.mkDerivation {
  name = "multi-word-tagger-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

