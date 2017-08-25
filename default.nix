{ mkDerivation, aeson, base, bifunctors, binary, discrimination, hashable, lens, stdenv, taggy-lens, text }:
mkDerivation {
  pname = "nlp-types";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bifunctors binary discrimination hashable lens taggy-lens text ];
  license = "unknown";
}
