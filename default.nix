{ revision }:

(import ./reflex-platform {}).project ({ pkgs, ... }:

let

  fasttext = import (revision.uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit (pkgs) stdenv fetchgit; };

  corenlp_pkgs =
    import (revision.uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
      inherit (pkgs) fetchurl fetchzip srcOnly;
    };

  env-hook-gen =
    haskellPackages:
      import (revision.uphere-nix-overlay + "/nix/env/corenlp.nix") {
        inherit (pkgs) makeSetupHook writeText;
        inherit haskellPackages;
        corenlp = corenlp_pkgs.corenlp;
        corenlp_models = corenlp_pkgs.corenlp_models;
      };

  hsconfig = pkgs.lib.callPackageWith
               (pkgs//revision)
               (revision.uphere-nix-overlay + "/nix/haskell-modules/configuration-language-engine.nix")
               { inherit fasttext;
                 corenlp = corenlp_pkgs.corenlp;
                 corenlp_models = corenlp_pkgs.corenlp_models;
               };

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

  tools = ghc: let env-hook = env-hook-gen ghc;
               in [ env-hook ];  # NOTE: you cannot have non-variable in this list.

  shells = {
    ghc = [
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
