{ mkDerivation, aeson, base, bifunctors, binary, discrimination, lens, stdenv, taggy-lens, text }:
mkDerivation {
  pname = "nlp-types";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bifunctors binary discrimination lens taggy-lens text ];
  license = "unknown";
}
