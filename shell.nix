{ pkgs ? import <nixpkgs> {}
, nlp-types ? <nlp-types>
}:

with pkgs;

let 
    config = 
      self: super: {
        "nlp-types" = self.callPackage (import nlp-types) {};
      };
    #hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    #newHaskellPackages = haskellPackages;
    newHaskellPackages = haskellPackages.override { overrides = self: super: config self super; };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              aeson
	      haskeline
              text
              #vector_0_12_0_1 # Specify the version explicitly since vector is 0.11.0.0 as of May 2017.
              vector-algorithms
              tasty-hunit
              containers
              attoparsec
              p.nlp-types
            ]);
in stdenv.mkDerivation {
  name = "corenlp-aeson-dev";
  buildInputs = [ hsenv 
                ];
  shellHook = ''
     PS1="\n\[\033[0;35m\][\u@\h.devel:\w]\$\[\033[0m\] "
  '';
}
