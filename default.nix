{ mkDerivation, attoparsec, base, cassava, containers, graph-algorithms, nlp-types, primitive
, stdenv, tasty, tasty-hunit, text, vector, vector-algorithms
, xxhash
, foreign-store, raw-strings-qq, mwc-random    
}:
mkDerivation {
  pname = "wiki-ner";
  version = "0.1.0.0";
  src = builtins.filterSource

  (path: type: type != "directory" || baseNameOf path != "dist" || baseNameOf path != ".cabal-sandbox")
  ./.;
  
  libraryHaskellDepends = [
    attoparsec base cassava containers graph-algorithms nlp-types primitive tasty-hunit text
    vector vector-algorithms xxhash
  ];
  executableHaskellDepends = [
    raw-strings-qq
    mwc-random    
    foreign-store
  ];

  testHaskellDepends = [
    attoparsec base containers nlp-types primitive tasty tasty-hunit
    text vector vector-algorithms xxhash
  ];
  license = stdenv.lib.licenses.unfree;
  doHaddock = false;
  doCheck = false;
}
