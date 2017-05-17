{ mkDerivation, base, containers, filepath, haskeline, lens
, monad-loops, optparse-applicative, split, stdenv, text
, text-format, unordered-containers
}:
mkDerivation {
  pname = "HWordNet";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers lens split text text-format unordered-containers
  ];
  executableHaskellDepends = [
    base filepath haskeline lens monad-loops optparse-applicative text
  ];
  description = "WordNet database parser and query engine";
  license = stdenv.lib.licenses.unfree;
}
