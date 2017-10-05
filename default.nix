{ mkDerivation, aeson, attoparsec, base, binary, boxes, bytestring
, containers, data-default, directory, directory-tree, either
, extra, filepath, hashable, haskeline, HCoreNLP, HCoreNLP-Proto
, HFrameNet, HWordNet, jni, jvm, lens, lexicon, monad-loops, mtl
, nlp-types, optparse-applicative, PropBank, protocol-buffers
, scientific, split, stdenv
# , syntactic-analysis
, taggy-lens, text
, textview, time, transformers, unordered-containers, vector
, VerbNet, wiki-ner
}:
mkDerivation {
  pname = "OntoNotes";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base binary boxes bytestring containers
    data-default directory directory-tree either extra filepath
    haskeline HCoreNLP HCoreNLP-Proto HFrameNet HWordNet jni jvm lens
    lexicon monad-loops mtl nlp-types PropBank protocol-buffers
    scientific
    #syntactic-analysis
    taggy-lens text textview time
    unordered-containers vector VerbNet wiki-ner
  ];
  executableHaskellDepends = [
    attoparsec base boxes containers directory directory-tree either
    extra filepath hashable haskeline HFrameNet HWordNet lens lexicon
    nlp-types optparse-applicative PropBank split
    #syntactic-analysis
    taggy-lens text transformers unordered-containers VerbNet
  ];
  license = "unknown";
}
