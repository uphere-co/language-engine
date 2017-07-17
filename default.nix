{ mkDerivation, async, base, binary, binary-orphans, containers
, directory, filepath, haskeline, lens, monad-loops
, optparse-applicative, split, stdenv, taggy-lens, text, time
, unordered-containers
}:
mkDerivation {
  pname = "HFrameNet";
  version = "0.1.0.0";
  src = builtins.filterSource
    (path: type: type != "directory" || baseNameOf path != "dist" || baseNameOf path != ".cabal-sandbox" || baseNameOf path != "run")
    ./.;

  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base binary binary-orphans containers directory filepath
    haskeline lens monad-loops split taggy-lens text time
    unordered-containers
  ];
  executableHaskellDepends = [ base filepath optparse-applicative ];
  license = stdenv.lib.licenses.bsd3;
}
