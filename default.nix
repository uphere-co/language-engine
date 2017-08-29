{ mkDerivation, attoparsec, base, bifunctors, bindings-svm, boxes
, bytestring, containers, data-default, discrimination, either
, fastText, filepath, foreign-store, HCoreNLP, HCoreNLP-Proto, HFrameNet, jni
, jvm, lens, nlp-types, optparse-applicative, OntoNotes, PropBank, split
, stdenv
, syntactic-analysis
, tasty, tasty-hunit, text, textview, time, transformers, vector
, wiki-ner
}:
mkDerivation {
  pname = "semantic-role-labeler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bifunctors bindings-svm boxes bytestring containers
    data-default discrimination either fastText filepath foreign-store
    HCoreNLP HCoreNLP-Proto HFrameNet jni jvm lens nlp-types OntoNotes PropBank
    syntactic-analysis
    split text time vector textview wiki-ner
  ];
  executableHaskellDepends = [
    attoparsec base bytestring containers data-default either fastText
    filepath foreign-store HCoreNLP HCoreNLP-Proto jni jvm lens
    nlp-types optparse-applicative PropBank syntactic-analysis
    text time transformers
    vector
  ];
  testHaskellDepends = [
    base containers HCoreNLP lens nlp-types tasty tasty-hunit text
  ];
  license = "unknown";
}
