{ mkDerivation, aeson, attoparsec, base, binary, boxes, bytestring
, containers, directory, directory-tree, either, extra, filepath
, haskeline, HCoreNLP, HCoreNLP-Proto, HFrameNet, HWordNet, jni
, jvm, lens, lexicon, nlp-types, OntoNotes, optparse-applicative
, PropBank, stdenv, syntactic-analysis, taggy-lens, text, time
, transformers, unordered-containers, VerbNet
}:
mkDerivation {
  pname = "lexicon-builder";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring containers directory either extra
    filepath HCoreNLP HCoreNLP-Proto HFrameNet HWordNet jni jvm lens
    lexicon nlp-types OntoNotes PropBank syntactic-analysis taggy-lens
    text time unordered-containers VerbNet
  ];
  executableHaskellDepends = [
    attoparsec base boxes containers directory directory-tree either
    extra filepath haskeline HFrameNet HWordNet lens lexicon nlp-types
    OntoNotes optparse-applicative PropBank syntactic-analysis text
    transformers unordered-containers
  ];
  license = "unknown";
}
