{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
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

  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-semantic-parser-api.nix")
               { inherit corenlp corenlp_models fasttext fetchgit fetchurl haskellPackages jdk stdenv;
                 haskellLib = haskell.lib;
                 pkgs = newpkgs;
               };

  newHaskellPackages = haskellPackages.override { overrides = hsconfig; };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            aeson-pretty
            attoparsec
            bifunctors
            bindings-svm
            blaze-html
            cabal-install
            either
            errors
            haskeline
            html-entities
            lens
            monad-loops
            optparse-applicative
            split
            text text-format
            yayaml
            lens
            mtl
            taggy-lens
            fficxx
            fficxx-runtime
            foreign-store
            p.lexicon
            p.lexicon-builder
            p.multi-word-tagger
            p.nlp-types
            p.OntoNotes
            p.PropBank
            p.HCoreNLP
            p.HCoreNLP-Proto
            p.HUKB-driver
            p.semantic-types
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
