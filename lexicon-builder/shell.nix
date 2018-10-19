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
  fasttext = import (uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit stdenv fetchgit; };

  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  hsconfig = lib.callPackageWith (pkgs//revision) (uphere-nix-overlay + "/nix/haskell-modules/configuration-semantic-parser-api.nix")
               { inherit corenlp corenlp_models fasttext; };

  newHaskellPackages = haskellPackages.override { overrides = hsconfig; };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            cabal-install
            aeson

            attoparsec
            bifunctors
            bindings-DSL

            boxes
            discrimination
            directory-tree
            haskeline
            lens
            monad-loops
            optparse-applicative
            split
            taggy-lens
            text
            text-format
            yayaml

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
    export CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.2.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';

}
