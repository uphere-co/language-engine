{ mkDerivation, aeson, base, stdenv, text }:
mkDerivation {
  pname = "nlp-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base text ];
  license = "unknown";
}
