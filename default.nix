{ mkDerivation, attoparsec, base, bifunctors, bindings-svm
, bytestring, containers, data-default, discrimination, either
, fastText, filepath, foreign-store, HCoreNLP, HCoreNLP-Proto, jni
, jvm, lens, nlp-types, optparse-applicative, PropBank, split
, stdenv, text, time, transformers, vector
, textview
}:
mkDerivation {
  pname = "semantic-role-labeler";
  version = "0.1.0.0";
  src = builtins.filterSource (path: type: type != "directory" || baseNameOf path != "dist" || baseNameOf path != ".cabal-sandbox") ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bifunctors bindings-svm bytestring containers
    data-default discrimination either fastText filepath foreign-store
    HCoreNLP HCoreNLP-Proto jni jvm lens nlp-types PropBank split text
    time vector textview
  ];
  executableHaskellDepends = [
    attoparsec base bytestring containers data-default either fastText
    filepath foreign-store HCoreNLP HCoreNLP-Proto jni jvm lens
    nlp-types optparse-applicative PropBank text time transformers
    vector
  ];
  license = "unknown";
  doHaddock = false;
  doCheck = false;
}
