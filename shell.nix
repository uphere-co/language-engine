{ pkgs               ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, HCoreNLP           ? <HCoreNLP>
, nlp-types          ? <nlp-types>
, PropBank           ? <PropBank>
, VerbNet            ? <VerbNet>
, wiki-ner           ? <wiki-ner>
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
      "HCoreNLP-Proto" = self.callPackage (import (HCoreNLP + "/HCoreNLP-Proto")) {};
      "HCoreNLP" = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
      "wiki-ner" = self.callPackage (import wiki-ner) {};
    
      "nlp-types" = self.callPackage (import nlp-types) {};
      "PropBank" = self.callPackage (import PropBank) {};
      "VerbNet" = self.callPackage (import VerbNet) {};
    };
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // config2 self super;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            cabal-install

            attoparsec
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
            
            p.nlp-types
            p.PropBank
            p.VerbNet
            p.wiki-ner
          ]);

in

stdenv.mkDerivation {
  name = "OntoNotes-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
    export OMP_NUM_THREADS=12
    export CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
  
}
