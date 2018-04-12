{ revision }:

with revision;

let pkgs0 = import nixpkgs { config.allowUnfree = true; };

    pkgs = import pkgs0.path {
                overlays = [ (self: super: {
                               libsvm = import (uphere-nix-overlay + "/nix/cpp-modules/libsvm/default.nix") { inherit (self) stdenv fetchurl; };
                             })
                           ];
              };
in

with pkgs;

let
  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  hsconfig = lib.callPackageWith (pkgs//revision) (uphere-nix-overlay + "/nix/haskell-modules/configuration-semantic-parser-api.nix") {
    inherit corenlp corenlp_models;
    fasttext = null;
    haskellLib = haskell.lib;
  };

  newHaskellPackages = haskellPackages.override { overrides = hsconfig; };

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
            p.HCoreNLP
            p.HCoreNLP-Proto
            p.HWordNet
            p.wiki-ner
            p.textview
          ]);

in

stdenv.mkDerivation {
  name = "syntactic-analysis-dev";
  buildInputs = [ hsenv libsvm ];
  shellHook = ''
    export OMP_NUM_THREADS=12
    export CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
 
}

