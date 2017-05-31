{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let 
    #hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    newHaskellPackages = haskellPackages; # haskellPackages.override { overrides = hsconfig; };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              aeson
	            haskeline
            ]);  
in
rec {
  wiki-ner     = newHaskellPackages.callPackage ./default.nix {};
}
