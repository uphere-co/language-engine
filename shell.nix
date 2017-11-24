{ pkgs               ? import <nixpkgs> {}
, graph-algorithms   ? <graph-algorithms>
, nlp-types          ? <nlp-types>
, uphere-nix-overlay ? <uphere-nix-overlay>
}:

with pkgs;

let     
    config1 = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { inherit pkgs; };
    config2 =
      self: super: {
#        mkDerivation = args: super.mkDerivation (args // {
#                         enableLibraryProfiling = true;
#                       });
        "nlp-types" = self.callPackage (import nlp-types) {};
        "graph-algorithms" = self.callPackage (import graph-algorithms) {};
      };
    newHaskellPackages = haskellPackages.override {
                           overrides = self: super: config1 self super // config2 self super;
                         };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              aeson
              aeson-pretty
              cassava
              haskeline
              text
              vector
              vector-algorithms
              tasty-hunit
              containers
              attoparsec
              xxhash
              binary
              mwc-random
              split
              lens
              raw-strings-qq
              foreign-store
              deepseq
              p.graph-algorithms
              p.nlp-types
            ]);
in stdenv.mkDerivation {
  name = "wiki-ner-dev";
  buildInputs = [ 
                  hsenv 
                  p7zip
                  lbzip2
                ];
  shellHook = ''
  '';
}
