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
            containers
            haskeline
            monad-loops
            p.nlp-types
            p.textview
            cabal-install
          ]);

in

stdenv.mkDerivation {
  name = "multi-word-tagger-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

