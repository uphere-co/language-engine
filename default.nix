{ mkDerivation, aeson, attoparsec, base, bifunctors, binary, boxes
, bytestring, containers, directory, directory-tree, extra
, filepath, haskeline, HCoreNLP, HCoreNLP-Proto, HFrameNet
, HWordNet, jni, jvm, lens, lexicon, nlp-types, OntoNotes
, optparse-applicative, PropBank, split, stdenv, syntactic-analysis
, taggy-lens, text, time, transformers, transformers-either
, unordered-containers, VerbNet
}:
mkDerivation {
  pname = "lexicon-builder";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring containers directory extra filepath
    HCoreNLP HCoreNLP-Proto HFrameNet HWordNet jni jvm lens lexicon
    nlp-types OntoNotes PropBank syntactic-analysis taggy-lens text
    time transformers-either unordered-containers VerbNet
  ];
  executableHaskellDepends = [
    aeson attoparsec base bifunctors boxes containers directory
    directory-tree extra filepath haskeline HFrameNet HWordNet lens
    lexicon nlp-types OntoNotes optparse-applicative PropBank split
    syntactic-analysis text transformers transformers-either
    unordered-containers
  ];
  license = "unknown";
}
