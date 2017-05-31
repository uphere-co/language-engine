{ pkgs ? import <nixpkgs> {}
, nlp-types ? <nlp-types>
}:

with pkgs;

let 
    nlptypes = newHaskellPackages.callPackage (import nlp-types) {};
    #hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    newHaskellPackages = haskellPackages; # haskellPackages.override { overrides = hsconfig; };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              aeson
	            haskeline
            ]);  
in
rec {
  wiki-ner     = newHaskellPackages.callPackage ./default.nix { nlp-types=nlptypes; };
}
