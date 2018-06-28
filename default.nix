{ revision }:



(import ./reflex-platform {}).project ({ pkgs, ... }:

let

  fasttext = import (revision.uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit (pkgs) stdenv fetchgit; };
  res_corenlp = import (revision.uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit (pkgs) fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  hsconfig = pkgs.lib.callPackageWith (pkgs//revision) (revision.uphere-nix-overlay + "/nix/haskell-modules/configuration-language-engine.nix")
               { inherit corenlp corenlp_models fasttext; };

in


{
  packages = {
    graph-algorithms      = ./graph-algorithms;
    HFrameNet             = ./HFrameNet;
    lexicon               = ./lexicon;
    lexicon-builder       = ./lexicon-builder;
    multi-word-tagger     = ./multi-word-tagger;
    OntoNotes             = ./OntoNotes;
    PropBank              = ./PropBank;
    semantic-role-labeler = ./semantic-role-labeler;
    semantic-types        = ./semantic-types;
    syntactic-analysis    = ./syntactic-analysis;
    time-tagger           = ./time-tagger;
    VerbNet               = ./VerbNet;
    wiki-ner              = ./wiki-ner;
  };


  overrides = hsconfig;

  tools = ghc:
            # TODO: move this code to uphere-nix-overlay
            let corenlpenv = pkgs.makeSetupHook { }
                  (pkgs.writeText "setup-hook.sh" ''
                     export CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${ghc.HCoreNLP}/share/x86_64-linux-ghc-8.2.1/HCoreNLP-0.1.0.0/HCoreNLPProto.jar"
                  '');
            in [ corenlpenv ghc.hserv ] ;

  shells = {
    ghc8_2_1 = [ 
                 "graph-algorithms"
                 "HFrameNet"
                 "lexicon"
                 "lexicon-builder"
                 "multi-word-tagger"
                 "OntoNotes"
                 "PropBank"
                 "semantic-role-labeler"
                 "semantic-types"
                 "syntactic-analysis"
                 "time-tagger"
                 "VerbNet"
                 "wiki-ner"
               ];
  };
})


