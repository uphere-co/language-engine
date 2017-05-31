{ mkDerivation, aeson, base, bifunctors, stdenv, text }:
mkDerivation {
  pname = "nlp-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bifunctors text ];
  license = "unknown";
}
