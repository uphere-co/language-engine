{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, fetchfin           ? <fetchfin>
, HCoreNLP           ? <HCoreNLP>
, HFrameNet          ? <HFrameNet>
, HUKB               ? <HUKB>
, HWordNet           ? <HWordNet>
, nlp-types          ? <nlp-types>
, predicate-matrix   ? <predicate-matrix>
, PropBank           ? <PropBank>
#, semantic-role-labeler ? <semantic-role-labeler>
, syntactic-analysis ? <syntactic-analysis>
, VerbNet            ? <VerbNet>
, wiki-ner           ? <wiki-ner>
, textview           ? <textview>
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

  config1 = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { pkgs = newpkgs; };

  haskellPackages1 = haskellPackages.override { overrides = config1; };

  #fastTextNix = import (semantic-role-labeler + "/fasttext/default.nix") {
  #  inherit stdenv;
  #  haskellPackages = haskellPackages1;
  #};

  config2 =
    self: super: {
      "nlp-types" = self.callPackage (import nlp-types) {};
      "textview" = self.callPackage (import textview) {};
      "newsapi" = self.callPackage (import (fetchfin + "/newsapi")) {};
      "HCoreNLP" = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
      "HCoreNLP-Proto" = self.callPackage (import (HCoreNLP + "/HCoreNLP-Proto")) {};
      "HFrameNet" = self.callPackage (import (builtins.filterSource (path: type: baseNameOf path != "run") HFrameNet)) {};      
      "HWordNet" = self.callPackage (import HWordNet) {};
      "predicate-matrix" = self.callPackage (import predicate-matrix) {};
      "PropBank" = self.callPackage (import PropBank) {};
      # "semantic-role-labeler" = self.callPackage (import semantic-role-labeler) {};
      "syntactic-analysis" = self.callPackage (import syntactic-analysis) {};
      "wiki-ner" = self.callPackage (import wiki-ner) {};
      "fastText" = self.callPackage fastTextNix { inherit fasttext; };
      "VerbNet" = self.callPackage (import VerbNet) {};
  };
  
  ukb = import (uphere-nix-overlay + "/nix/cpp-modules/ukb.nix") { inherit stdenv fetchgit fetchurl boost; };
  config3 = import (HUKB + "/HUKB-driver/config.nix") { pkgs = newpkgs; inherit uphere-nix-overlay ukb; };
  config4 =
    self: super: {
      "HUKB-driver" = self.callPackage (import (HUKB + "/HUKB-driver")) {};
    };
  myhaskellpkgs = haskell.packages.ghc802.override {
    overrides = self: super: config1 self super // config2 self super // config3 self super // config4 self super;
  };



  hsenv = myhaskellpkgs.ghcWithPackages (p: with p; [
            aeson
            attoparsec
            bifunctors
            bindings-DSL
            boxes
            fficxx
            fficxx-runtime
            foreign-store
            lens
            text
            cabal-install
            p.newsapi
            p.HCoreNLP
            p.HFrameNet
            p.HWordNet
            p.PropBank
            p.syntactic-analysis
            # p.semantic-role-labeler
            p.textview
            p.VerbNet
          ]);

in

stdenv.mkDerivation {
  name = "ontonotes-srl-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
    export CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
'';
}
