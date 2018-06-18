{ mkDerivation, base, stdenv, formatting, hashable, lens, text, nlp-types }:
mkDerivation {
  pname = "lexicon";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base formatting hashable lens text nlp-types ];
  license = "unknown";
}
