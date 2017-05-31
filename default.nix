{ mkDerivation, base
, containers, text, vector-algorithms
, tasty-hunit
, nlp-types
}:

mkDerivation {
  pname = "wiki-ner";
  version = "0.1.0.0";
  src = builtins.filterSource
    (path: type: type != "directory" || baseNameOf path != "dist" || baseNameOf path != ".cabal-sandbox")
    ./.;

  libraryHaskellDepends = [
    containers text vector-algorithms tasty-hunit
    nlp-types
  ];
  executableHaskellDepends = [
    containers text vector-algorithms tasty-hunit
    nlp-types
  ];
  testHaskellDepends = [
    containers text vector-algorithms tasty-hunit
    nlp-types
  ];
  license = "unknown";
  doHaddock = false;
  doCheck   = false;
}
