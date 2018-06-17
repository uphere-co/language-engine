{ mkDerivation, attoparsec, base, bifunctors, boxes
, bytestring, containers, data-default, discrimination, either, errors
, fastText, filepath, foreign-store, html-entities, HCoreNLP, HCoreNLP-Proto, HFrameNet, HUKB-driver, jni
, jvm, lens, lexicon-builder, multi-word-tagger, nlp-types
, optparse-applicative, OntoNotes, PropBank, split
, stdenv
, semantic-types, syntactic-analysis
, tasty, tasty-hunit, text, text-format, textview, time, transformers, vector
, wiki-ner
}:
mkDerivation {
  pname = "semantic-role-labeler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bifunctors boxes bytestring containers
    data-default discrimination either errors fastText filepath foreign-store
    html-entities HCoreNLP HCoreNLP-Proto HFrameNet jni jvm lens lexicon-builder multi-word-tagger
    nlp-types OntoNotes PropBank
    syntactic-analysis
    split text text-format time vector textview wiki-ner
  ];
  executableHaskellDepends = [
    attoparsec base bytestring containers data-default either fastText
    filepath foreign-store HCoreNLP HCoreNLP-Proto jni jvm lens
    lexicon-builder nlp-types optparse-applicative PropBank
    semantic-types syntactic-analysis
    text time transformers
    vector HUKB-driver
  ];
  testHaskellDepends = [
    base containers HCoreNLP lens nlp-types tasty tasty-hunit text
  ];
  license = "unknown";
  doHaddock = false;

}