{ pkgs               ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, graph-algorithms   ? <graph-algorithms>
, HCoreNLP           ? <HCoreNLP>
, HFrameNet          ? <HFrameNet>
, HWordNet           ? <HWordNet>
, lexicon            ? <lexicon>
, nlp-types          ? <nlp-types>
, OntoNotes          ? <OntoNotes>
, PropBank           ? <PropBank>
, syntactic-analysis ? <syntactic-analysis>
, VerbNet            ? <VerbNet>
, wiki-ner           ? <wiki-ner>
, textview           ? <textview>
}:

with pkgs;

let
  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
               { inherit pkgs; };

  config2 =
    self: super: {
      "graph-algorithms" = self.callPackage (import graph-algorithms) {};
      "HCoreNLP-Proto" = self.callPackage (import (HCoreNLP + "/HCoreNLP-Proto")) {};
      "HCoreNLP"       = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
      "HFrameNet"      = self.callPackage (import (builtins.filterSource (path: type: baseNameOf path != "run") HFrameNet)) {};
      "HWordNet"       = self.callPackage (import (builtins.filterSource (path: type: baseNameOf path != "run") HWordNet)) {};
      "lexicon"        = self.callPackage (import (builtins.filterSource (path: type: baseNameOf path != "dist") lexicon)) {};
      "nlp-types" = self.callPackage (import nlp-types) {};
      "OntoNotes" = self.callPackage (import OntoNotes) {};
      "PropBank"  = self.callPackage (import PropBank) {};
      "syntactic-analysis" = self.callPackage (import syntactic-analysis) {};
      "textview"  = self.callPackage (import textview) {};
      "VerbNet"   = self.callPackage (import VerbNet) {};
      "wiki-ner"  = self.callPackage (import wiki-ner) {};

    };
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // config2 self super;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            cabal-install
            aeson

            attoparsec
            bifunctors
            bindings-DSL

            boxes
            discrimination
            directory-tree
            either
            # extra
            haskeline
            lens
            monad-loops
            optparse-applicative
            split
            text text-format
            yayaml

            lens
            taggy-lens
            p.HCoreNLP
            p.HCoreNLP-Proto
            p.HFrameNet
            p.HWordNet
            p.lexicon
            p.nlp-types
            p.OntoNotes
            p.PropBank
            p.syntactic-analysis
            p.VerbNet
            p.wiki-ner
          ]);

in

stdenv.mkDerivation {
  name = "lexicon-builder-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
    export OMP_NUM_THREADS=12
    export CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';

}
