{ mkDerivation, base, bifunctors, containers, errors, HWordNet, lens, monad-loops, lexicon, nlp-types
, PropBank, stdenv
, text, textview
, tasty, tasty-hunit
, formatting
}:
mkDerivation {
  pname = "syntactic-analysis";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors containers errors formatting HWordNet lens monad-loops
    lexicon
    nlp-types PropBank text textview
  ];
  testHaskellDepends = [
    base containers HWordNet lens nlp-types tasty tasty-hunit text
  ];
  license = "unknown";
}
