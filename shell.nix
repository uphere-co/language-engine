{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let #hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    newHaskellPackages = haskellPackages; # haskellPackages.override { overrides = hsconfig; };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              aeson
	      haskeline
              text
              #vector_0_12_0_1 # Specify the version explicitly since vector is 0.11.0.0 as of May 2017.
              vector-algorithms
              tasty-hunit
              containers
            ]);
in stdenv.mkDerivation {
  name = "corenlp-aeson-dev";
  buildInputs = [ hsenv 
                ];
  shellHook = ''
     PS1="\n\[\033[0;35m\][\u@\h.devel:\w]\$\[\033[0m\] "

  '';
}
