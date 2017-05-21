{ mkDerivation, base, lens, stdenv, taggy-lens, text }:
mkDerivation {
  pname = "PropBank";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens taggy-lens text ];
  license = stdenv.lib.licenses.unfree;
}
