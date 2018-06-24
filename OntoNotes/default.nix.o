{ mkDerivation, aeson, attoparsec, base, binary, boxes, bytestring
, containers, data-default, directory, directory-tree, extra
, filepath, haskeline, HCoreNLP, HCoreNLP-Proto, HFrameNet
, HWordNet, jni, jvm, lens, lexicon, monad-loops, nlp-types
, PropBank, protocol-buffers, scientific, stdenv, taggy-lens, text
, textview, time, transformers, transformers-either
, unordered-containers, vector, VerbNet, wiki-ner
}:
mkDerivation {
  pname = "OntoNotes";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base binary boxes bytestring containers
    data-default directory directory-tree extra filepath haskeline
    HCoreNLP HCoreNLP-Proto HFrameNet HWordNet jni jvm lens lexicon
    monad-loops nlp-types PropBank protocol-buffers scientific
    taggy-lens text textview time transformers transformers-either
    unordered-containers vector VerbNet wiki-ner
  ];
  license = "unknown";
}
