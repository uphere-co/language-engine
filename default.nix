{ mkDerivation, base, bifunctors, containers, HCoreNLP
, HCoreNLP-Proto, HWordNet, lens, monad-loops, lexicon, nlp-types
, PropBank, stdenv
, text, textview
, tasty, tasty-hunit
}:
mkDerivation {
  pname = "syntactic-analysis";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors containers HCoreNLP HCoreNLP-Proto HWordNet lens monad-loops
    lexicon
    nlp-types PropBank text textview
  ];
  testHaskellDepends = [
    base containers HCoreNLP lens nlp-types tasty tasty-hunit text
  ];
  license = "unknown";
}
