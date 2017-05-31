{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, HCoreNLP ? <HCoreNLP>
, nlp-types ? <nlp-types>
, PropBank ? <PropBank>
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
  hsconfig2 =
    self: super: {
      "nlp-types" = self.callPackage (import nlp-types) {};
      "PropBank" = self.callPackage (import PropBank) {};
      "HCoreNLP-Proto" = self.callPackage (import (HCoreNLP + "/HCoreNLP-Proto")) {};
      "HCoreNLP" = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
    };  
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // hsconfig2 self super;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            attoparsec
            bifunctors
            either
            haskeline
            lens
            monad-loops
            optparse-applicative
            split
            text text-format
            yayaml

            lens
            taggy-lens

            p.nlp-types
            p.PropBank
            p.HCoreNLP
            p.HCoreNLP-Proto
          ]);

in

stdenv.mkDerivation {
  name = "SRL-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
    export CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
}

