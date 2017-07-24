{ pkgs               ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, HCoreNLP           ? <HCoreNLP>
, nlp-types          ? <nlp-types>
}:

with pkgs;

let
  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
               { inherit pkgs; };
  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;
  config1 = self: super: {
    "HCoreNLP" = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
    "HCoreNLP-Proto" = self.callPackage (import (HCoreNLP + "/HCoreNLP-Proto")) {};
    "nlp-types" = self.callPackage (import nlp-types) {};
  };
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // config1 self super;
  };
  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            cabal-install
            p.HCoreNLP
            p.HCoreNLP-Proto
            p.nlp-types
          ]);

in

stdenv.mkDerivation {
  name = "time-tagger-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

