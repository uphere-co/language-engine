{ mkDerivation, base, bifunctors, containers, HCoreNLP
, HCoreNLP-Proto, lens, monad-loops, nlp-types, stdenv, tasty
, tasty-hunit, text, textview
}:
mkDerivation {
  pname = "syntactic-analysis";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors containers HCoreNLP HCoreNLP-Proto lens monad-loops
    nlp-types text textview
  ];
  testHaskellDepends = [
    base containers HCoreNLP lens nlp-types tasty tasty-hunit text
  ];
  license = "unknown";
}
