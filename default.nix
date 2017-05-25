{ mkDerivation, attoparsec, base, directory, filepath, haskeline
, lens, monad-loops, optparse-applicative, stdenv, taggy-lens, text
, unordered-containers, yayaml
}:
mkDerivation {
  pname = "PropBank";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base directory filepath lens taggy-lens text
    unordered-containers
  ];
  executableHaskellDepends = [
    base filepath haskeline lens monad-loops optparse-applicative text
    unordered-containers yayaml
  ];
  description = "PropBank/NomBank parser and query engine";
  license = stdenv.lib.licenses.unfree;
}
