{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, nlp-types          ? <nlp-types>
}:

with pkgs;

let
  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
               { inherit pkgs; };

  hsconfig2 = self: super: {
      "nlp-types" = self.callPackage (import nlp-types) {};
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // hsconfig2 self super;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            hashable
            lens
            p.nlp-types
            text
            cabal-install
          ]);

in

stdenv.mkDerivation {
  name = "lexicon-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

