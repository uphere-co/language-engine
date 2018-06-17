{ mkDerivation, stdenv
, base
, binary
, bytestring
, primitive
, text
, vector
, vector-algorithms
, containers
, xxhash
}:
mkDerivation {
  pname = "graph-algorithms";
  version = "0.1.0.0";
  src = builtins.filterSource

  (path: type: type != "directory" || baseNameOf path != "dist" || baseNameOf path != ".cabal-sandbox")
  ./.;
  
  libraryHaskellDepends = [
    base
    binary
    bytestring
    primitive
    text
    vector
    vector-algorithms
    containers
    xxhash
  ];
  executableHaskellDepends = [
  ];

  testHaskellDepends = [
  ];
  license = stdenv.lib.licenses.unfree;
  doHaddock = false;
  doCheck = false;
}
