{ mkDerivation, base, stdenv, hashable, lens, text, nlp-types }:
mkDerivation {
  pname = "lexicon";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hashable lens text nlp-types ];
  license = "unknown";
}
