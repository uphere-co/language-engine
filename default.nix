{ mkDerivation, base, stdenv, text }:
mkDerivation {
  pname = "lexicon";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base text ];
  license = "unknown";
}
