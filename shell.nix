{ pkgs               ? import <nixpkgs> {}
, HCoreNLP           ? <HCoreNLP>
, nlp-types          ? <nlp-types>
, textview           ? <textview>
, uphere-nix-overlay ? <uphere-nix-overlay>
}:

with pkgs;

let
  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
               { inherit pkgs; };
  config2 =
    self: super: {
      "nlp-types" = self.callPackage (import nlp-types) {};
      "textview" = self.callPackage (import textview) {};
    };
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // config2 self super;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            attoparsec
            containers
            haskeline
            monad-loops
            p.nlp-types
            p.textview
            cabal-install
          ]);

in

stdenv.mkDerivation {
  name = "multi-word-tagger-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

