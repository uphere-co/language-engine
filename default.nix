{ mkDerivation, base, bytestring, HCoreNLP, HCoreNLP-Proto, jvm
, lens, nlp-types, protocol-buffers, stdenv, text, time
}:
mkDerivation {
  pname = "time-tagger";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring HCoreNLP HCoreNLP-Proto jvm lens nlp-types
    protocol-buffers text time
  ];
  license = stdenv.lib.licenses.unfree;
}
