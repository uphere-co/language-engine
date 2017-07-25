{ mkDerivation, attoparsec, base, bifunctors, bindings-svm
, bytestring, containers, data-default, discrimination, either
, fastText, filepath, foreign-store, HCoreNLP, HCoreNLP-Proto, jni
, jvm, lens, nlp-types, optparse-applicative, PropBank, split
, stdenv, tasty, tasty-hunit, text, textview, time, transformers
, vector
}:
mkDerivation {
  pname = "semantic-role-labeler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bifunctors bindings-svm bytestring containers
    data-default discrimination either fastText filepath foreign-store
    HCoreNLP HCoreNLP-Proto jni jvm lens nlp-types PropBank split text
    textview time vector
  ];
  executableHaskellDepends = [
    attoparsec base bytestring containers data-default either fastText
    filepath foreign-store HCoreNLP HCoreNLP-Proto jni jvm lens
    nlp-types optparse-applicative PropBank text time transformers
    vector
  ];
  testHaskellDepends = [
    base containers HCoreNLP lens nlp-types tasty tasty-hunit text
  ];
  license = "unknown";
}
