{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, nlp-types
}:

with pkgs;

let
  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
               { inherit pkgs; };

  config2 =
    self: super: {
      "nlp-types" = self.callPackage (import nlp-types) {};
    };
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // config2 self super;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            attoparsec
            discrimination
            haskeline
            lens
            monad-loops
            optparse-applicative
            split
            text text-format
            yayaml

            lens
            taggy-lens
            p.nlp-types
          ]);

in

stdenv.mkDerivation {
  name = "PropBank-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

