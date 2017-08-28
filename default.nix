{ mkDerivation, aeson, base, bifunctors, binary, discrimination, hashable
, lens, monad-loops, stdenv, taggy-lens, text }:
mkDerivation {
  pname = "nlp-types";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base bifunctors binary discrimination hashable
                            lens monad-loops taggy-lens text ];
  license = "unknown";
}
