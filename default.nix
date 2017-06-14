{ mkDerivation, aeson, base, bifunctors, lens, stdenv, text }:
mkDerivation {
  pname = "nlp-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bifunctors lens text ];
  license = "unknown";
}
