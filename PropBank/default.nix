{ mkDerivation, attoparsec, base, containers, directory
, discrimination, filepath, haskeline, lens, monad-loops, nlp-types
, optparse-applicative, split, stdenv, taggy-lens, text
, transformers, unordered-containers, yayaml
, tasty-hunit
, tasty
}:
mkDerivation {
  pname = "PropBank";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base directory discrimination filepath lens nlp-types
    split taggy-lens text transformers unordered-containers yayaml
  ];
  executableHaskellDepends = [
    attoparsec base containers discrimination filepath haskeline lens
    monad-loops nlp-types optparse-applicative split text
    unordered-containers
  ];
  testHaskellDepends = [ tasty-hunit tasty ];
  description = "PropBank/NomBank parser and query engine";
  license = stdenv.lib.licenses.unfree;
}
