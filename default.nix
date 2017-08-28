{ mkDerivation, base, containers, either, nlp-types, stdenv, text, textview
, transformers
}:
mkDerivation {
  pname = "multi-word-tagger";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers either nlp-types text textview transformers
  ];
  license = "unknown";
}
