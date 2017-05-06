{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let

  #myhaskellpkgs = haskell.packages.ghc802.override {
  #  overrides = self: super: config1 self super // config2 self super;
  #}; 

  hsenv = haskellPackages.ghcWithPackages (p: with p; [
          ]);

in

stdenv.mkDerivation {
  name = "HFrameNet-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

