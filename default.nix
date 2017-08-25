{ mkDerivation, base, containers, either, stdenv, text, textview
, transformers
}:
mkDerivation {
  pname = "multi-word-tagger";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers either text textview transformers
  ];
  license = "unknown";
}
