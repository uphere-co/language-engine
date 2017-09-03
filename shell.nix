{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, HCoreNLP           ? <HCoreNLP>
, HFrameNet          ? <HFrameNet>
, HWordNet           ? <HWordNet>
, graph-algorithms   ? <graph-algorithms>
, lexicon            ? <lexicon>
, multi-word-tagger  ? <multi-word-tagger>
, nlp-types          ? <nlp-types>
, OntoNotes          ? <OntoNotes>
, PropBank           ? <PropBank>
, syntactic-analysis ? <syntactic-analysis>
, textview           ? <textview>
, VerbNet            ? <VerbNet>
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
  fasttext = import (uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit stdenv fetchgit; };

  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { pkgs = newpkgs; };

  haskellPackages1 = haskellPackages.override { overrides = hsconfig; };

  fastTextNix = import ./fasttext/default.nix {
    inherit stdenv;
    haskellPackages = haskellPackages1;
  };

  hsconfig2 =
    self: super: {
      "lexicon"        = self.callPackage (import lexicon) {};
      "multi-word-tagger" = self.callPackage (import multi-word-tagger) {};
      "nlp-types"      = self.callPackage (import nlp-types) {};
      "HCoreNLP-Proto" = self.callPackage (import (HCoreNLP + "/HCoreNLP-Proto")) {};
      "HCoreNLP"       = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
      "HFrameNet"      = self.callPackage (import HFrameNet) {};
      "HWordNet"       = self.callPackage (import HWordNet) {};
      "graph-algorithms" = self.callPackage (import graph-algorithms) {};
      "OntoNotes"      = self.callPackage (import OntoNotes) {};
      "PropBank"       = self.callPackage (import PropBank) {};
      "VerbNet"        = self.callPackage (import VerbNet) {};
      "wiki-ner"       = self.callPackage (import wiki-ner) {};
      "fastText"       = self.callPackage fastTextNix { inherit fasttext; };
      "syntactic-analysis" = self.callPackage (import syntactic-analysis) {};
      "textview"       = self.callPackage (import textview) {};
    };  
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // hsconfig2 self super;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            attoparsec
            bifunctors
            bindings-svm
            cabal-install
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
            p.multi-word-tagger
            p.nlp-types
            p.OntoNotes
            p.PropBank
            p.HCoreNLP
            p.HCoreNLP-Proto
            p.syntactic-analysis
            p.textview            
            p.wiki-ner
            fastText
          ]);

in

stdenv.mkDerivation {
  name = "SRL-dev";
  buildInputs = [ hsenv fasttext libsvm graphviz ];
  shellHook = ''
    export OMP_NUM_THREADS=12
    export CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
}

