{ revision }:

with revision;

let

  pkgs = import nixpkgs { config.allowUnfree = true; };

in 

with pkgs;

let

  fasttext = import (uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit stdenv fetchgit; };

  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  hsconfig = lib.callPackageWith (pkgs//revision) (uphere-nix-overlay + "/nix/haskell-modules/configuration-language-engine.nix")
               { inherit corenlp corenlp_models fasttext;
               };


  newHaskellPackages = pkgs.haskell.packages.ghc821.override { overrides = hsconfig; };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            aeson-pretty
            async
            base-prelude
            binary-orphans
            blaze-html
            boxes
            cassava
            cmdargs
            directory-tree
            formatting
            foreign-store
            haskell-src-exts
            html-entities
            lens            
            mwc-random
            raw-strings-qq
            taggy-lens
            tasty
            tasty-hunit
            transformers-compat
            unordered-containers
            vector-algorithms
            xxhash
            #
            fastText
            HCoreNLP-Proto
            HCoreNLP
            HUKB-driver
            textview
          ]);
in 

stdenv.mkDerivation {
  name = "language-engine-dev";
  buildInputs = [ hsenv fasttext ];
  shellHook = ''
    export OMP_NUM_THREADS=12
    export CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
  

}