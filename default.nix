{ mkDerivation, base, directory, filepath, lens, nlp-types, stdenv
, taggy-lens, text
}:
mkDerivation {
  pname = "VerbNet";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base directory filepath lens nlp-types taggy-lens text
  ];
  license = "unknown";
}
