{ stdenv, haskellPackages }:

let 
    hsenv = haskellPackages.ghcWithPackages (p: with p; [ fficxx-runtime fficxx ]);
in

stdenv.mkDerivation {
  name = "fastText-src";
  buildInputs = [ hsenv ];
  src = ./.; 
  buildPhase = ''
    ghc fastTextGen.hs
    ./fastTextGen vectorwrapper.h wrapper.cc
  '';
  installPhase = ''
    mkdir -p $out
    cp -a fastText/* $out
  '';

}
