{ mkDerivation, aeson, base, bifunctors, lens, stdenv, taggy-lens, text }:
mkDerivation {
  pname = "nlp-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bifunctors lens taggy-lens text ];
  license = "unknown";
}
