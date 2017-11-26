{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, graph-algorithms   ? <graph-algorithms>
, HWordNet           ? <HWordNet>
, lexicon            ? <lexicon>
, nlp-types          ? <nlp-types>
, PropBank           ? <PropBank>
, textview           ? <textview>
, wiki-ner           ? <wiki-ner>
}:


let newpkgs = import pkgs.path { 
                overlays = [ (self: super: {
                               libsvm = import (uphere-nix-overlay + "/nix/cpp-modules/libsvm/default.nix") { inherit (self) stdenv fetchurl; };
                             })
                           ];
              };
in

with newpkgs;

let

  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { pkgs = newpkgs; };

  haskellPackages1 = haskellPackages.override { overrides = hsconfig; };


  hsconfig2 =
    self: super: {
      "graph-algorithms" = self.callPackage (import graph-algorithms) {};
      "HWordNet"         = self.callPackage (import HWordNet) {};
      "lexicon"          = self.callPackage (import lexicon) {};
      "nlp-types"        = self.callPackage (import nlp-types) {};
      "PropBank"         = self.callPackage (import PropBank) {};
      "textview"         = self.callPackage (import textview) {};
      "wiki-ner"         = self.callPackage (import wiki-ner) {};      
    };  
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // hsconfig2 self super;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            attoparsec
            bifunctors
            bindings-svm
            cabal-install
            directory-tree
            either
            haskeline
            lens
            monad-loops
            optparse-applicative
            split
            #svm-simple
            text text-format
            yayaml

            lens
            taggy-lens

            fficxx
            fficxx-runtime

            foreign-store
            
            p.lexicon
            p.nlp-types
            p.PropBank
            p.HWordNet
            p.wiki-ner
            p.textview
          ]);

in

stdenv.mkDerivation {
  name = "syntactic-analysis-dev";
  buildInputs = [ hsenv libsvm ];
}

