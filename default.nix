{ mkDerivation, attoparsec, base, containers, nlp-types, primitive
, stdenv, tasty, tasty-hunit, text, vector, vector-algorithms
, xxhash
}:
mkDerivation {
  pname = "wiki-ner";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base containers nlp-types primitive tasty-hunit text
    vector vector-algorithms xxhash
  ];
  testHaskellDepends = [
    attoparsec base containers nlp-types primitive tasty tasty-hunit
    text vector vector-algorithms xxhash
  ];
  license = stdenv.lib.licenses.unfree;
  doCheck = false;
}
