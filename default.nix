{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "nlp-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = "unknown";
}
