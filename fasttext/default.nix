{ stdenv, haskellPackages }:

let
  fastText-src = import ./gen.nix { inherit stdenv haskellPackages; };
in 

{ mkDerivation, base 
, fficxx, fficxx-runtime, stdenv, template-haskell, stdcxx
, fasttext
}:
mkDerivation {
  pname = "fastText";
  version = "0.0";
  src = fastText-src;
  libraryHaskellDepends = [
    base fficxx fficxx-runtime template-haskell stdcxx
  ];
  librarySystemDepends = [ fasttext ];
  license = stdenv.lib.licenses.bsd3;
}
