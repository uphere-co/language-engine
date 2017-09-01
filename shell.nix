{ pkgs ? import <nixpkgs> {}
, nlp-types ? <nlp-types>
, graph-algorithms ? <graph-algorithms>
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

    #hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    #newHaskellPackages = haskellPackages;
    newHaskellPackages = haskellPackages.override {
                           overrides = self: super: config1 self super // config2 self super;
                         };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              aeson
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
  name = "corenlp-aeson-dev";
  buildInputs = [ 
                  hsenv 
                  p7zip
                  lbzip2
                ];
  shellHook = ''
     PS1="\n\[\033[0;35m\][\u@\h.devel:\w]\$\[\033[0m\] "
  '';
}
