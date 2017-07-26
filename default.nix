{ mkDerivation, base, bifunctors, containers, HCoreNLP
, HCoreNLP-Proto, lens, nlp-types, stdenv, text, textview
}:
mkDerivation {
  pname = "syntactic-analysis";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors containers HCoreNLP HCoreNLP-Proto lens nlp-types
    text textview
  ];
  license = "unknown";
}
